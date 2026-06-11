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
!||    my_alloc_impl_idx4_mod   ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_tools_mod       ../common_source/tools/memory/my_alloc_tools.F90
!||--- called by  -----------------------------------------------------
!||    my_alloc_mod             ../common_source/tools/memory/my_alloc.F90
!||====================================================================
! ======================================================================================================================
! fypp template — generates my_alloc_impl_idx4.F90
! Do NOT edit the generated my_alloc_impl_idx4.F90 directly; edit this file and re-run fypp.
!
! Contains all allocation subroutines with integer (32-bit) dimension arguments.
! The integer(8)-dimension counterparts are in my_alloc_impl_idx8.fy.
!
! To regenerate:
!   fypp my_alloc_impl_idx4.fy my_alloc_impl_idx4.F90
! ======================================================================================================================

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



      module my_alloc_impl_idx4_mod
        use iso_c_binding,      only : c_loc
        use my_alloc_tools_mod, only : my_alloc_check, record_alloc_addr
        use elbufdef_mod, only : elbuf_struct_, g_bufel_, l_bufel_, buf_prop_, buf_nloc_, &
          buf_nlocts_, buf_damp_range_, buf_eos_, buf_poro_, buf_visc_, buf_xfem_, &
          fail_loc_, buf_fail_, buf_mat_, l_bufel_dir_, buf_intloc_, buf_intlay_, buf_lay_
        implicit none

        private :: my_alloc_real_1d
        private :: my_alloc_real_2d
        private :: my_alloc_real_3d
        private :: my_alloc_double_1d
        private :: my_alloc_double_2d
        private :: my_alloc_double_3d
        private :: my_alloc_integer_1d
        private :: my_alloc_integer_2d
        private :: my_alloc_integer_3d
        private :: my_alloc_logical_1d
        private :: my_alloc_logical_2d
        private :: my_alloc_logical_3d
        private :: my_alloc_elbuf_1d
        private :: my_alloc_gbuf_1d
        private :: my_alloc_lbuf_1d
        private :: my_alloc_bufprop_1d
        private :: my_alloc_bufnloc_1d
        private :: my_alloc_bufnlocts_1d
        private :: my_alloc_bufdamp_1d
        private :: my_alloc_bufeos_1d
        private :: my_alloc_bufporo_1d
        private :: my_alloc_bufvisc_1d
        private :: my_alloc_bufxfem_1d
        private :: my_alloc_failloc_1d
        private :: my_alloc_buffail_1d
        private :: my_alloc_bufmat_1d
        private :: my_alloc_lbufdir_1d
        private :: my_alloc_bufintloc_1d
        private :: my_alloc_bufintlay_1d
        private :: my_alloc_buflay_1d
        private :: my_alloc_preal_1d
        private :: my_alloc_preal_2d
        private :: my_alloc_preal_3d
        private :: my_alloc_pdouble_1d
        private :: my_alloc_pdouble_2d
        private :: my_alloc_pdouble_3d
        private :: my_alloc_pinteger_1d
        private :: my_alloc_pinteger_2d
        private :: my_alloc_pinteger_3d
        private :: my_alloc_plogical_1d
        private :: my_alloc_plogical_2d
        private :: my_alloc_plogical_3d
        private :: my_alloc_pelbuf_1d
        private :: my_alloc_pgbuf_1d
        private :: my_alloc_plbuf_1d
        private :: my_alloc_pbufprop_1d
        private :: my_alloc_pbufnloc_1d
        private :: my_alloc_pbufnlocts_1d
        private :: my_alloc_pbufdamp_1d
        private :: my_alloc_pbufeos_1d
        private :: my_alloc_pbufporo_1d
        private :: my_alloc_pbufvisc_1d
        private :: my_alloc_pbufxfem_1d
        private :: my_alloc_pfailloc_1d
        private :: my_alloc_pbuffail_1d
        private :: my_alloc_pbufmat_1d
        private :: my_alloc_plbufdir_1d
        private :: my_alloc_pbufintloc_1d
        private :: my_alloc_pbufintlay_1d
        private :: my_alloc_pbuflay_1d

        public :: my_alloc

        interface my_alloc
          module procedure my_alloc_real_1d
          module procedure my_alloc_real_2d
          module procedure my_alloc_real_3d
          module procedure my_alloc_double_1d
          module procedure my_alloc_double_2d
          module procedure my_alloc_double_3d
          module procedure my_alloc_integer_1d
          module procedure my_alloc_integer_2d
          module procedure my_alloc_integer_3d
          module procedure my_alloc_logical_1d
          module procedure my_alloc_logical_2d
          module procedure my_alloc_logical_3d
          module procedure my_alloc_elbuf_1d
          module procedure my_alloc_gbuf_1d
          module procedure my_alloc_lbuf_1d
          module procedure my_alloc_bufprop_1d
          module procedure my_alloc_bufnloc_1d
          module procedure my_alloc_bufnlocts_1d
          module procedure my_alloc_bufdamp_1d
          module procedure my_alloc_bufeos_1d
          module procedure my_alloc_bufporo_1d
          module procedure my_alloc_bufvisc_1d
          module procedure my_alloc_bufxfem_1d
          module procedure my_alloc_failloc_1d
          module procedure my_alloc_buffail_1d
          module procedure my_alloc_bufmat_1d
          module procedure my_alloc_lbufdir_1d
          module procedure my_alloc_bufintloc_1d
          module procedure my_alloc_bufintlay_1d
          module procedure my_alloc_buflay_1d
          module procedure my_alloc_preal_1d
          module procedure my_alloc_preal_2d
          module procedure my_alloc_preal_3d
          module procedure my_alloc_pdouble_1d
          module procedure my_alloc_pdouble_2d
          module procedure my_alloc_pdouble_3d
          module procedure my_alloc_pinteger_1d
          module procedure my_alloc_pinteger_2d
          module procedure my_alloc_pinteger_3d
          module procedure my_alloc_plogical_1d
          module procedure my_alloc_plogical_2d
          module procedure my_alloc_plogical_3d
          module procedure my_alloc_pelbuf_1d
          module procedure my_alloc_pgbuf_1d
          module procedure my_alloc_plbuf_1d
          module procedure my_alloc_pbufprop_1d
          module procedure my_alloc_pbufnloc_1d
          module procedure my_alloc_pbufnlocts_1d
          module procedure my_alloc_pbufdamp_1d
          module procedure my_alloc_pbufeos_1d
          module procedure my_alloc_pbufporo_1d
          module procedure my_alloc_pbufvisc_1d
          module procedure my_alloc_pbufxfem_1d
          module procedure my_alloc_pfailloc_1d
          module procedure my_alloc_pbuffail_1d
          module procedure my_alloc_pbufmat_1d
          module procedure my_alloc_plbufdir_1d
          module procedure my_alloc_pbufintloc_1d
          module procedure my_alloc_pbufintlay_1d
          module procedure my_alloc_pbuflay_1d
        end interface my_alloc

      contains

! ======================================================================================================================
!                          GENERATED ALLOCATION ROUTINES  (integer(4) dimension arguments)
!   Loop order: MEM_KINDS x TYPES x RANKS
! ======================================================================================================================

!! \brief Allocate a 1D real array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_real_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_real_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_real_1d

!! \brief Allocate a 2D real array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_real_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_real_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_real_2d

!! \brief Allocate a 3D real array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_real_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_real_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_real_3d

!! \brief Allocate a 1D double precision array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_double_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_double_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_double_1d

!! \brief Allocate a 2D double precision array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_double_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_double_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_double_2d

!! \brief Allocate a 3D double precision array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_double_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_double_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_double_3d

!! \brief Allocate a 1D integer array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_integer_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_integer_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_integer_1d

!! \brief Allocate a 2D integer array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_integer_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_integer_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_integer_2d

!! \brief Allocate a 3D integer array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_integer_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_integer_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_integer_3d

!! \brief Allocate a 1D logical array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_logical_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_logical_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_logical_1d

!! \brief Allocate a 2D logical array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_logical_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_logical_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_logical_2d

!! \brief Allocate a 3D logical array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_logical_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_logical_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :, :), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_logical_3d

!! \brief Allocate a 1D type(elbuf_struct_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_elbuf_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_elbuf_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(elbuf_struct_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_elbuf_1d

!! \brief Allocate a 1D type(g_bufel_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_gbuf_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_gbuf_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(g_bufel_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_gbuf_1d

!! \brief Allocate a 1D type(l_bufel_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_lbuf_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_lbuf_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(l_bufel_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_lbuf_1d

!! \brief Allocate a 1D type(buf_prop_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufprop_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufprop_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_prop_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufprop_1d

!! \brief Allocate a 1D type(buf_nloc_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufnloc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufnloc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_nloc_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufnloc_1d

!! \brief Allocate a 1D type(buf_nlocts_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufnlocts_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufnlocts_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_nlocts_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufnlocts_1d

!! \brief Allocate a 1D type(buf_damp_range_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufdamp_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufdamp_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_damp_range_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufdamp_1d

!! \brief Allocate a 1D type(buf_eos_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufeos_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufeos_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_eos_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufeos_1d

!! \brief Allocate a 1D type(buf_poro_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufporo_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufporo_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_poro_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufporo_1d

!! \brief Allocate a 1D type(buf_visc_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufvisc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufvisc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_visc_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufvisc_1d

!! \brief Allocate a 1D type(buf_xfem_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufxfem_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufxfem_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_xfem_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufxfem_1d

!! \brief Allocate a 1D type(fail_loc_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_failloc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_failloc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(fail_loc_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_failloc_1d

!! \brief Allocate a 1D type(buf_fail_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_buffail_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_buffail_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_fail_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_buffail_1d

!! \brief Allocate a 1D type(buf_mat_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufmat_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufmat_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_mat_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufmat_1d

!! \brief Allocate a 1D type(l_bufel_dir_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_lbufdir_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_lbufdir_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(l_bufel_dir_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_lbufdir_1d

!! \brief Allocate a 1D type(buf_intloc_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufintloc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufintloc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_intloc_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufintloc_1d

!! \brief Allocate a 1D type(buf_intlay_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_bufintlay_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_bufintlay_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_intlay_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_bufintlay_1d

!! \brief Allocate a 1D type(buf_lay_) array (allocatable, integer dims)
!||====================================================================
!||    my_alloc_buflay_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_buflay_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_lay_), dimension(:), allocatable, target, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_buflay_1d

!! \brief Allocate a 1D real array (pointer, integer dims)
!||====================================================================
!||    my_alloc_preal_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_preal_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_preal_1d

!! \brief Allocate a 2D real array (pointer, integer dims)
!||====================================================================
!||    my_alloc_preal_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_preal_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_preal_2d

!! \brief Allocate a 3D real array (pointer, integer dims)
!||====================================================================
!||    my_alloc_preal_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_preal_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          real, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_preal_3d

!! \brief Allocate a 1D double precision array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pdouble_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pdouble_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pdouble_1d

!! \brief Allocate a 2D double precision array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pdouble_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pdouble_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_pdouble_2d

!! \brief Allocate a 3D double precision array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pdouble_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pdouble_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          double precision, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_pdouble_3d

!! \brief Allocate a 1D integer array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pinteger_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pinteger_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pinteger_1d

!! \brief Allocate a 2D integer array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pinteger_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pinteger_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_pinteger_2d

!! \brief Allocate a 3D integer array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pinteger_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pinteger_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          integer, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_pinteger_3d

!! \brief Allocate a 1D logical array (pointer, integer dims)
!||====================================================================
!||    my_alloc_plogical_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_plogical_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_plogical_1d

!! \brief Allocate a 2D logical array (pointer, integer dims)
!||====================================================================
!||    my_alloc_plogical_2d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_plogical_2d(a, n, m, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
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
        end subroutine my_alloc_plogical_2d

!! \brief Allocate a 3D logical array (pointer, integer dims)
!||====================================================================
!||    my_alloc_plogical_3d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_plogical_3d(a, l, m, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          logical, dimension(:, :, :), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
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
        end subroutine my_alloc_plogical_3d

!! \brief Allocate a 1D type(elbuf_struct_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pelbuf_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pelbuf_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(elbuf_struct_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pelbuf_1d

!! \brief Allocate a 1D type(g_bufel_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pgbuf_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pgbuf_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(g_bufel_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pgbuf_1d

!! \brief Allocate a 1D type(l_bufel_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_plbuf_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_plbuf_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(l_bufel_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_plbuf_1d

!! \brief Allocate a 1D type(buf_prop_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufprop_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufprop_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_prop_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufprop_1d

!! \brief Allocate a 1D type(buf_nloc_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufnloc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufnloc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_nloc_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufnloc_1d

!! \brief Allocate a 1D type(buf_nlocts_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufnlocts_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufnlocts_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_nlocts_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufnlocts_1d

!! \brief Allocate a 1D type(buf_damp_range_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufdamp_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufdamp_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_damp_range_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufdamp_1d

!! \brief Allocate a 1D type(buf_eos_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufeos_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufeos_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_eos_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufeos_1d

!! \brief Allocate a 1D type(buf_poro_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufporo_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufporo_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_poro_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufporo_1d

!! \brief Allocate a 1D type(buf_visc_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufvisc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufvisc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_visc_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufvisc_1d

!! \brief Allocate a 1D type(buf_xfem_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufxfem_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufxfem_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_xfem_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufxfem_1d

!! \brief Allocate a 1D type(fail_loc_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pfailloc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pfailloc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(fail_loc_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pfailloc_1d

!! \brief Allocate a 1D type(buf_fail_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbuffail_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbuffail_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_fail_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbuffail_1d

!! \brief Allocate a 1D type(buf_mat_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufmat_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufmat_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_mat_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufmat_1d

!! \brief Allocate a 1D type(l_bufel_dir_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_plbufdir_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_plbufdir_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(l_bufel_dir_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_plbufdir_1d

!! \brief Allocate a 1D type(buf_intloc_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufintloc_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufintloc_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_intloc_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufintloc_1d

!! \brief Allocate a 1D type(buf_intlay_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbufintlay_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbufintlay_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_intlay_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbufintlay_1d

!! \brief Allocate a 1D type(buf_lay_) array (pointer, integer dims)
!||====================================================================
!||    my_alloc_pbuflay_1d
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine my_alloc_pbuflay_1d(a, n, msg, stat, lower_bound)
          use iso_c_binding, only: c_loc
          type(buf_lay_), dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
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
        end subroutine my_alloc_pbuflay_1d


      end module my_alloc_impl_idx4_mod
