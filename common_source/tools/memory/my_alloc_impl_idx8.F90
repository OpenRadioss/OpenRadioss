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
! fypp template — generates my_alloc_impl_idx8.F90
! Do NOT edit the generated my_alloc_impl_idx8.F90 directly; edit this file and re-run fypp.
!
! Contains all allocation subroutines with integer(8) (64-bit) dimension arguments.
! The integer(4)-dimension counterparts are in my_alloc_impl_idx4.fy.
!
! To regenerate:
!   fypp my_alloc_impl_idx8.fy my_alloc_impl_idx8.F90
! ======================================================================================================================

! ======================================================================================================================
! Shared fypp include for my_alloc_impl_idx4.fy and my_alloc_impl_idx8.fy
!
! Do NOT invoke fypp directly on this file — it is #:include'd by the two impl .fy files.
!
! Defines:
!   TYPES     : (fortran_type, subroutine_suffix)
!   MEM_KINDS : (fortran_attribute, name_prefix)   allocatable vs pointer
!   RANKS     : (rank_number, list_of_dim_variable_names)
!   alloc_sub : macro that emits one allocation subroutine
!
! IDX_KINDS must be set by the including file before this include.
! ======================================================================================================================



!||====================================================================
!||    my_alloc_impl_idx8_mod   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- called by ------------------------------------------------------
!||    my_alloc_mod             ../common_source/tools/memory/my_alloc.F90
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod             ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    my_alloc_tools_mod       ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
      module my_alloc_impl_idx8_mod
        use iso_c_binding,      only : c_loc
        use my_alloc_tools_mod, only : my_alloc_check, record_alloc_addr
        use elbufdef_mod, only : elbuf_struct_, g_bufel_, l_bufel_, buf_prop_, buf_nloc_, &
          buf_nlocts_, buf_damp_range_, buf_eos_, buf_poro_, buf_visc_, buf_xfem_, &
          fail_loc_, buf_fail_, buf_mat_, l_bufel_dir_, buf_intloc_, buf_intlay_, buf_lay_
        implicit none

        private :: my_alloc_8_real_1d
        private :: my_alloc_8_real_2d
        private :: my_alloc_8_real_3d
        private :: my_alloc_8_double_1d
        private :: my_alloc_8_double_2d
        private :: my_alloc_8_double_3d
        private :: my_alloc_8_integer_1d
        private :: my_alloc_8_integer_2d
        private :: my_alloc_8_integer_3d
        private :: my_alloc_8_logical_1d
        private :: my_alloc_8_logical_2d
        private :: my_alloc_8_logical_3d
        private :: my_alloc_8_preal_1d
        private :: my_alloc_8_preal_2d
        private :: my_alloc_8_preal_3d
        private :: my_alloc_8_pdouble_1d
        private :: my_alloc_8_pdouble_2d
        private :: my_alloc_8_pdouble_3d
        private :: my_alloc_8_pinteger_1d
        private :: my_alloc_8_pinteger_2d
        private :: my_alloc_8_pinteger_3d
        private :: my_alloc_8_plogical_1d
        private :: my_alloc_8_plogical_2d
        private :: my_alloc_8_plogical_3d

        public :: my_alloc

        interface my_alloc
          module procedure my_alloc_8_real_1d
          module procedure my_alloc_8_real_2d
          module procedure my_alloc_8_real_3d
          module procedure my_alloc_8_double_1d
          module procedure my_alloc_8_double_2d
          module procedure my_alloc_8_double_3d
          module procedure my_alloc_8_integer_1d
          module procedure my_alloc_8_integer_2d
          module procedure my_alloc_8_integer_3d
          module procedure my_alloc_8_logical_1d
          module procedure my_alloc_8_logical_2d
          module procedure my_alloc_8_logical_3d
          module procedure my_alloc_8_preal_1d
          module procedure my_alloc_8_preal_2d
          module procedure my_alloc_8_preal_3d
          module procedure my_alloc_8_pdouble_1d
          module procedure my_alloc_8_pdouble_2d
          module procedure my_alloc_8_pdouble_3d
          module procedure my_alloc_8_pinteger_1d
          module procedure my_alloc_8_pinteger_2d
          module procedure my_alloc_8_pinteger_3d
          module procedure my_alloc_8_plogical_1d
          module procedure my_alloc_8_plogical_2d
          module procedure my_alloc_8_plogical_3d
        end interface my_alloc

      contains

! ======================================================================================================================
!                          GENERATED ALLOCATION ROUTINES  (integer(8) dimension arguments)
!   Loop order: MEM_KINDS x TYPES x RANKS
! ======================================================================================================================

!! \brief Allocate a 1D real array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_real_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check       ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr    ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_real_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_real_1d

!! \brief Allocate a 2D real array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_real_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check       ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr    ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_real_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_real_2d

!! \brief Allocate a 3D real array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_real_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check       ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr    ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_real_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_real_3d

!! \brief Allocate a 1D double precision array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_double_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check         ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr      ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_double_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_double_1d

!! \brief Allocate a 2D double precision array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_double_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check         ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr      ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_double_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_double_2d

!! \brief Allocate a 3D double precision array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_double_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check         ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr      ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_double_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_double_3d

!! \brief Allocate a 1D integer array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_integer_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_integer_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_integer_1d

!! \brief Allocate a 2D integer array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_integer_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_integer_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_integer_2d

!! \brief Allocate a 3D integer array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_integer_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_integer_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_integer_3d

!! \brief Allocate a 1D logical array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_logical_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_logical_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_logical_1d

!! \brief Allocate a 2D logical array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_logical_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_logical_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_logical_2d

!! \brief Allocate a 3D logical array (allocatable, integer(8) dims)
!||====================================================================
!||    my_alloc_8_logical_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_logical_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_logical_3d

!! \brief Allocate a 1D real array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_preal_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check        ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr     ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_preal_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_preal_1d

!! \brief Allocate a 2D real array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_preal_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check        ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr     ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_preal_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_preal_2d

!! \brief Allocate a 3D real array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_preal_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check        ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr     ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_preal_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_preal_3d

!! \brief Allocate a 1D double precision array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_pdouble_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_pdouble_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_pdouble_1d

!! \brief Allocate a 2D double precision array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_pdouble_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_pdouble_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_pdouble_2d

!! \brief Allocate a 3D double precision array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_pdouble_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check          ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr       ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_pdouble_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_pdouble_3d

!! \brief Allocate a 1D integer array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_pinteger_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check           ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr        ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_pinteger_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_pinteger_1d

!! \brief Allocate a 2D integer array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_pinteger_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check           ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr        ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_pinteger_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_pinteger_2d

!! \brief Allocate a 3D integer array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_pinteger_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check           ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr        ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_pinteger_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_pinteger_3d

!! \brief Allocate a 1D logical array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_plogical_1d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check           ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr        ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_plogical_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_plogical_1d

!! \brief Allocate a 2D logical array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_plogical_2d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check           ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr        ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_plogical_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+n-1, lb_:lb_+m-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_plogical_2d

!! \brief Allocate a 3D logical array (pointer, integer(8) dims)
!||====================================================================
!||    my_alloc_8_plogical_3d   ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check           ../common_source/tools/memory/my_alloc_tools.F90
!||    record_alloc_addr        ../common_source/tools/memory/my_move_alloc.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine my_alloc_8_plogical_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(lb_:lb_+l-1, lb_:lb_+m-1, lb_:lb_+n-1), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))), msg, &
            int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine my_alloc_8_plogical_3d


      end module my_alloc_impl_idx8_mod
