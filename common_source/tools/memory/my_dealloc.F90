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
! fypp template — generates my_dealloc.F90
! Do NOT edit the generated my_dealloc.F90 directly; edit this file and re-run fypp.
!
! Deallocation counterpart to my_alloc.fy.
!
! Each generated subroutine:
!   1. Guards against double-free (checks allocated / associated).
!   2. Resolves the pointer address with c_loc of the first element.
!   3. Calls cpp_record_dealloc_addr(addr) — the C++ side looks up the address in
!      the per-allocation map, subtracts the previously recorded byte count from the
!      per-site counter, and removes the address entry.  One site name can have many
!      live addresses simultaneously (same allocation site called from many callers).
!   4. Deallocates the array.  For pointer arrays, also nullifies after free.
!
! Axes of variation:
!   TYPES     : (fortran_type, short_name)  — same set as my_alloc.fy
!   MEM_KINDS : (fortran_attr, name_prefix) — 'allocatable' / 'pointer'
!   RANKS     : (rank, dim_var_list)        — 1D / 2D / 3D
!
! No IDX_KINDS axis: deallocation takes only the array, no dimension arguments.
!
! Subroutine naming convention:
!   my_dealloc_<mem_prefix><type_name>_<rank>d
!   e.g. my_dealloc_pdouble_3d  =>  pointer, double precision, 3D
!
! Placeholder for derived types is at the bottom of the contains section.
! ======================================================================================================================



      module my_dealloc_mod
        use iso_c_binding, only : c_ptr, c_loc
        use elbufdef_mod, only : elbuf_struct_, g_bufel_, l_bufel_, buf_prop_, buf_nloc_, &
          buf_nlocts_, buf_damp_range_, buf_eos_, buf_poro_, buf_visc_, buf_xfem_, &
          fail_loc_, buf_fail_, buf_mat_, l_bufel_dir_, buf_intloc_, buf_intlay_, buf_lay_
        implicit none

        interface
          subroutine cpp_record_dealloc_addr(addr) bind(C, name="cpp_record_dealloc_addr")
            use iso_c_binding, only : c_ptr
            type(c_ptr), value, intent(in) :: addr
          end subroutine cpp_record_dealloc_addr
        end interface

        private :: my_dealloc_real_1d
        private :: my_dealloc_real_2d
        private :: my_dealloc_real_3d
        private :: my_dealloc_double_1d
        private :: my_dealloc_double_2d
        private :: my_dealloc_double_3d
        private :: my_dealloc_integer_1d
        private :: my_dealloc_integer_2d
        private :: my_dealloc_integer_3d
        private :: my_dealloc_logical_1d
        private :: my_dealloc_logical_2d
        private :: my_dealloc_logical_3d
        private :: my_dealloc_preal_1d
        private :: my_dealloc_preal_2d
        private :: my_dealloc_preal_3d
        private :: my_dealloc_pdouble_1d
        private :: my_dealloc_pdouble_2d
        private :: my_dealloc_pdouble_3d
        private :: my_dealloc_pinteger_1d
        private :: my_dealloc_pinteger_2d
        private :: my_dealloc_pinteger_3d
        private :: my_dealloc_plogical_1d
        private :: my_dealloc_plogical_2d
        private :: my_dealloc_plogical_3d
        private :: my_dealloc_pelbuf_1d
        private :: my_dealloc_pelbuf_2d
        private :: my_dealloc_pelbuf_3d
        private :: my_dealloc_pgbuf_1d
        private :: my_dealloc_pgbuf_2d
        private :: my_dealloc_pgbuf_3d
        private :: my_dealloc_plbuf_1d
        private :: my_dealloc_plbuf_2d
        private :: my_dealloc_plbuf_3d
        private :: my_dealloc_pbufprop_1d
        private :: my_dealloc_pbufprop_2d
        private :: my_dealloc_pbufprop_3d
        private :: my_dealloc_pbufnloc_1d
        private :: my_dealloc_pbufnloc_2d
        private :: my_dealloc_pbufnloc_3d
        private :: my_dealloc_pbufnlocts_1d
        private :: my_dealloc_pbufnlocts_2d
        private :: my_dealloc_pbufnlocts_3d
        private :: my_dealloc_pbufdamp_1d
        private :: my_dealloc_pbufdamp_2d
        private :: my_dealloc_pbufdamp_3d
        private :: my_dealloc_pbufeos_1d
        private :: my_dealloc_pbufeos_2d
        private :: my_dealloc_pbufeos_3d
        private :: my_dealloc_pbufporo_1d
        private :: my_dealloc_pbufporo_2d
        private :: my_dealloc_pbufporo_3d
        private :: my_dealloc_pbufvisc_1d
        private :: my_dealloc_pbufvisc_2d
        private :: my_dealloc_pbufvisc_3d
        private :: my_dealloc_pbufxfem_1d
        private :: my_dealloc_pbufxfem_2d
        private :: my_dealloc_pbufxfem_3d
        private :: my_dealloc_pfailloc_1d
        private :: my_dealloc_pfailloc_2d
        private :: my_dealloc_pfailloc_3d
        private :: my_dealloc_pbuffail_1d
        private :: my_dealloc_pbuffail_2d
        private :: my_dealloc_pbuffail_3d
        private :: my_dealloc_pbufmat_1d
        private :: my_dealloc_pbufmat_2d
        private :: my_dealloc_pbufmat_3d
        private :: my_dealloc_plbufdir_1d
        private :: my_dealloc_plbufdir_2d
        private :: my_dealloc_plbufdir_3d
        private :: my_dealloc_pbufintloc_1d
        private :: my_dealloc_pbufintloc_2d
        private :: my_dealloc_pbufintloc_3d
        private :: my_dealloc_pbufintlay_1d
        private :: my_dealloc_pbufintlay_2d
        private :: my_dealloc_pbufintlay_3d
        private :: my_dealloc_pbuflay_1d
        private :: my_dealloc_pbuflay_2d
        private :: my_dealloc_pbuflay_3d

        public :: my_dealloc

        interface my_dealloc
          module procedure my_dealloc_real_1d
          module procedure my_dealloc_real_2d
          module procedure my_dealloc_real_3d
          module procedure my_dealloc_double_1d
          module procedure my_dealloc_double_2d
          module procedure my_dealloc_double_3d
          module procedure my_dealloc_integer_1d
          module procedure my_dealloc_integer_2d
          module procedure my_dealloc_integer_3d
          module procedure my_dealloc_logical_1d
          module procedure my_dealloc_logical_2d
          module procedure my_dealloc_logical_3d
          module procedure my_dealloc_preal_1d
          module procedure my_dealloc_preal_2d
          module procedure my_dealloc_preal_3d
          module procedure my_dealloc_pdouble_1d
          module procedure my_dealloc_pdouble_2d
          module procedure my_dealloc_pdouble_3d
          module procedure my_dealloc_pinteger_1d
          module procedure my_dealloc_pinteger_2d
          module procedure my_dealloc_pinteger_3d
          module procedure my_dealloc_plogical_1d
          module procedure my_dealloc_plogical_2d
          module procedure my_dealloc_plogical_3d
          module procedure my_dealloc_pelbuf_1d
          module procedure my_dealloc_pelbuf_2d
          module procedure my_dealloc_pelbuf_3d
          module procedure my_dealloc_pgbuf_1d
          module procedure my_dealloc_pgbuf_2d
          module procedure my_dealloc_pgbuf_3d
          module procedure my_dealloc_plbuf_1d
          module procedure my_dealloc_plbuf_2d
          module procedure my_dealloc_plbuf_3d
          module procedure my_dealloc_pbufprop_1d
          module procedure my_dealloc_pbufprop_2d
          module procedure my_dealloc_pbufprop_3d
          module procedure my_dealloc_pbufnloc_1d
          module procedure my_dealloc_pbufnloc_2d
          module procedure my_dealloc_pbufnloc_3d
          module procedure my_dealloc_pbufnlocts_1d
          module procedure my_dealloc_pbufnlocts_2d
          module procedure my_dealloc_pbufnlocts_3d
          module procedure my_dealloc_pbufdamp_1d
          module procedure my_dealloc_pbufdamp_2d
          module procedure my_dealloc_pbufdamp_3d
          module procedure my_dealloc_pbufeos_1d
          module procedure my_dealloc_pbufeos_2d
          module procedure my_dealloc_pbufeos_3d
          module procedure my_dealloc_pbufporo_1d
          module procedure my_dealloc_pbufporo_2d
          module procedure my_dealloc_pbufporo_3d
          module procedure my_dealloc_pbufvisc_1d
          module procedure my_dealloc_pbufvisc_2d
          module procedure my_dealloc_pbufvisc_3d
          module procedure my_dealloc_pbufxfem_1d
          module procedure my_dealloc_pbufxfem_2d
          module procedure my_dealloc_pbufxfem_3d
          module procedure my_dealloc_pfailloc_1d
          module procedure my_dealloc_pfailloc_2d
          module procedure my_dealloc_pfailloc_3d
          module procedure my_dealloc_pbuffail_1d
          module procedure my_dealloc_pbuffail_2d
          module procedure my_dealloc_pbuffail_3d
          module procedure my_dealloc_pbufmat_1d
          module procedure my_dealloc_pbufmat_2d
          module procedure my_dealloc_pbufmat_3d
          module procedure my_dealloc_plbufdir_1d
          module procedure my_dealloc_plbufdir_2d
          module procedure my_dealloc_plbufdir_3d
          module procedure my_dealloc_pbufintloc_1d
          module procedure my_dealloc_pbufintloc_2d
          module procedure my_dealloc_pbufintloc_3d
          module procedure my_dealloc_pbufintlay_1d
          module procedure my_dealloc_pbufintlay_2d
          module procedure my_dealloc_pbufintlay_3d
          module procedure my_dealloc_pbuflay_1d
          module procedure my_dealloc_pbuflay_2d
          module procedure my_dealloc_pbuflay_3d
        end interface my_dealloc

      contains

        subroutine record_dealloc_addr(addr)
          use iso_c_binding, only : c_ptr
          type(c_ptr), intent(in) :: addr
          call cpp_record_dealloc_addr(addr)
        end subroutine record_dealloc_addr

! ======================================================================================================================
!                                     GENERATED DEALLOCATION ROUTINES
!   Loop order: MEM_KINDS x TYPES x RANKS
!   MEM_KINDS : allocatable (''), pointer ('p')
!   TYPES     : real, double precision, integer, logical
!   RANKS     : 1d, 2d, 3d
! ======================================================================================================================

!! \brief Deallocate a 1D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_1d(a)
          real, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_1d

!! \brief Deallocate a 2D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_2d(a)
          real, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_2d

!! \brief Deallocate a 3D real array (allocatable)
!||====================================================================
!||    my_dealloc_real_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_real_3d(a)
          real, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_real_3d

!! \brief Deallocate a 1D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_1d(a)
          double precision, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_1d

!! \brief Deallocate a 2D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_2d(a)
          double precision, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_2d

!! \brief Deallocate a 3D double precision array (allocatable)
!||====================================================================
!||    my_dealloc_double_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_double_3d(a)
          double precision, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_double_3d

!! \brief Deallocate a 1D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_1d(a)
          integer, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_1d

!! \brief Deallocate a 2D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_2d(a)
          integer, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_2d

!! \brief Deallocate a 3D integer array (allocatable)
!||====================================================================
!||    my_dealloc_integer_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_integer_3d(a)
          integer, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_integer_3d

!! \brief Deallocate a 1D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_1d(a)
          logical, dimension(:), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_1d

!! \brief Deallocate a 2D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_2d(a)
          logical, dimension(:, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_2d

!! \brief Deallocate a 3D logical array (allocatable)
!||====================================================================
!||    my_dealloc_logical_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_logical_3d(a)
          logical, dimension(:, :, :), allocatable, target, intent(inout) :: a
          if (allocated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
          end if
        end subroutine my_dealloc_logical_3d

!! \brief Deallocate a 1D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_1d(a)
          real, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_1d

!! \brief Deallocate a 2D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_2d(a)
          real, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_2d

!! \brief Deallocate a 3D real array (pointer)
!||====================================================================
!||    my_dealloc_preal_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_preal_3d(a)
          real, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_preal_3d

!! \brief Deallocate a 1D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_1d(a)
          double precision, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_1d

!! \brief Deallocate a 2D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_2d(a)
          double precision, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_2d

!! \brief Deallocate a 3D double precision array (pointer)
!||====================================================================
!||    my_dealloc_pdouble_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pdouble_3d(a)
          double precision, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pdouble_3d

!! \brief Deallocate a 1D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_1d(a)
          integer, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_1d

!! \brief Deallocate a 2D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_2d(a)
          integer, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_2d

!! \brief Deallocate a 3D integer array (pointer)
!||====================================================================
!||    my_dealloc_pinteger_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pinteger_3d(a)
          integer, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pinteger_3d

!! \brief Deallocate a 1D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_1d(a)
          logical, dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_1d

!! \brief Deallocate a 2D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_2d(a)
          logical, dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_2d

!! \brief Deallocate a 3D logical array (pointer)
!||====================================================================
!||    my_dealloc_plogical_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plogical_3d(a)
          logical, dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plogical_3d

!! \brief Deallocate a 1D type(elbuf_struct_) array (pointer)
!||====================================================================
!||    my_dealloc_pelbuf_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pelbuf_1d(a)
          type(elbuf_struct_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pelbuf_1d

!! \brief Deallocate a 2D type(elbuf_struct_) array (pointer)
!||====================================================================
!||    my_dealloc_pelbuf_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pelbuf_2d(a)
          type(elbuf_struct_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pelbuf_2d

!! \brief Deallocate a 3D type(elbuf_struct_) array (pointer)
!||====================================================================
!||    my_dealloc_pelbuf_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pelbuf_3d(a)
          type(elbuf_struct_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pelbuf_3d

!! \brief Deallocate a 1D type(g_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_pgbuf_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pgbuf_1d(a)
          type(g_bufel_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pgbuf_1d

!! \brief Deallocate a 2D type(g_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_pgbuf_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pgbuf_2d(a)
          type(g_bufel_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pgbuf_2d

!! \brief Deallocate a 3D type(g_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_pgbuf_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pgbuf_3d(a)
          type(g_bufel_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pgbuf_3d

!! \brief Deallocate a 1D type(l_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_plbuf_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbuf_1d(a)
          type(l_bufel_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbuf_1d

!! \brief Deallocate a 2D type(l_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_plbuf_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbuf_2d(a)
          type(l_bufel_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbuf_2d

!! \brief Deallocate a 3D type(l_bufel_) array (pointer)
!||====================================================================
!||    my_dealloc_plbuf_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbuf_3d(a)
          type(l_bufel_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbuf_3d

!! \brief Deallocate a 1D type(buf_prop_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufprop_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufprop_1d(a)
          type(buf_prop_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufprop_1d

!! \brief Deallocate a 2D type(buf_prop_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufprop_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufprop_2d(a)
          type(buf_prop_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufprop_2d

!! \brief Deallocate a 3D type(buf_prop_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufprop_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufprop_3d(a)
          type(buf_prop_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufprop_3d

!! \brief Deallocate a 1D type(buf_nloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnloc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnloc_1d(a)
          type(buf_nloc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnloc_1d

!! \brief Deallocate a 2D type(buf_nloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnloc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnloc_2d(a)
          type(buf_nloc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnloc_2d

!! \brief Deallocate a 3D type(buf_nloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnloc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnloc_3d(a)
          type(buf_nloc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnloc_3d

!! \brief Deallocate a 1D type(buf_nlocts_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnlocts_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnlocts_1d(a)
          type(buf_nlocts_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnlocts_1d

!! \brief Deallocate a 2D type(buf_nlocts_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnlocts_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnlocts_2d(a)
          type(buf_nlocts_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnlocts_2d

!! \brief Deallocate a 3D type(buf_nlocts_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufnlocts_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufnlocts_3d(a)
          type(buf_nlocts_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufnlocts_3d

!! \brief Deallocate a 1D type(buf_damp_range_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufdamp_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufdamp_1d(a)
          type(buf_damp_range_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufdamp_1d

!! \brief Deallocate a 2D type(buf_damp_range_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufdamp_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufdamp_2d(a)
          type(buf_damp_range_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufdamp_2d

!! \brief Deallocate a 3D type(buf_damp_range_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufdamp_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufdamp_3d(a)
          type(buf_damp_range_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufdamp_3d

!! \brief Deallocate a 1D type(buf_eos_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufeos_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufeos_1d(a)
          type(buf_eos_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufeos_1d

!! \brief Deallocate a 2D type(buf_eos_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufeos_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufeos_2d(a)
          type(buf_eos_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufeos_2d

!! \brief Deallocate a 3D type(buf_eos_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufeos_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufeos_3d(a)
          type(buf_eos_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufeos_3d

!! \brief Deallocate a 1D type(buf_poro_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufporo_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufporo_1d(a)
          type(buf_poro_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufporo_1d

!! \brief Deallocate a 2D type(buf_poro_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufporo_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufporo_2d(a)
          type(buf_poro_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufporo_2d

!! \brief Deallocate a 3D type(buf_poro_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufporo_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufporo_3d(a)
          type(buf_poro_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufporo_3d

!! \brief Deallocate a 1D type(buf_visc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufvisc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufvisc_1d(a)
          type(buf_visc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufvisc_1d

!! \brief Deallocate a 2D type(buf_visc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufvisc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufvisc_2d(a)
          type(buf_visc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufvisc_2d

!! \brief Deallocate a 3D type(buf_visc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufvisc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufvisc_3d(a)
          type(buf_visc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufvisc_3d

!! \brief Deallocate a 1D type(buf_xfem_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufxfem_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufxfem_1d(a)
          type(buf_xfem_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufxfem_1d

!! \brief Deallocate a 2D type(buf_xfem_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufxfem_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufxfem_2d(a)
          type(buf_xfem_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufxfem_2d

!! \brief Deallocate a 3D type(buf_xfem_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufxfem_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufxfem_3d(a)
          type(buf_xfem_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufxfem_3d

!! \brief Deallocate a 1D type(fail_loc_) array (pointer)
!||====================================================================
!||    my_dealloc_pfailloc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pfailloc_1d(a)
          type(fail_loc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pfailloc_1d

!! \brief Deallocate a 2D type(fail_loc_) array (pointer)
!||====================================================================
!||    my_dealloc_pfailloc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pfailloc_2d(a)
          type(fail_loc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pfailloc_2d

!! \brief Deallocate a 3D type(fail_loc_) array (pointer)
!||====================================================================
!||    my_dealloc_pfailloc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pfailloc_3d(a)
          type(fail_loc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pfailloc_3d

!! \brief Deallocate a 1D type(buf_fail_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuffail_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuffail_1d(a)
          type(buf_fail_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuffail_1d

!! \brief Deallocate a 2D type(buf_fail_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuffail_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuffail_2d(a)
          type(buf_fail_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuffail_2d

!! \brief Deallocate a 3D type(buf_fail_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuffail_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuffail_3d(a)
          type(buf_fail_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuffail_3d

!! \brief Deallocate a 1D type(buf_mat_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufmat_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufmat_1d(a)
          type(buf_mat_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufmat_1d

!! \brief Deallocate a 2D type(buf_mat_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufmat_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufmat_2d(a)
          type(buf_mat_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufmat_2d

!! \brief Deallocate a 3D type(buf_mat_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufmat_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufmat_3d(a)
          type(buf_mat_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufmat_3d

!! \brief Deallocate a 1D type(l_bufel_dir_) array (pointer)
!||====================================================================
!||    my_dealloc_plbufdir_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbufdir_1d(a)
          type(l_bufel_dir_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbufdir_1d

!! \brief Deallocate a 2D type(l_bufel_dir_) array (pointer)
!||====================================================================
!||    my_dealloc_plbufdir_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbufdir_2d(a)
          type(l_bufel_dir_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbufdir_2d

!! \brief Deallocate a 3D type(l_bufel_dir_) array (pointer)
!||====================================================================
!||    my_dealloc_plbufdir_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_plbufdir_3d(a)
          type(l_bufel_dir_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_plbufdir_3d

!! \brief Deallocate a 1D type(buf_intloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintloc_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintloc_1d(a)
          type(buf_intloc_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintloc_1d

!! \brief Deallocate a 2D type(buf_intloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintloc_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintloc_2d(a)
          type(buf_intloc_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintloc_2d

!! \brief Deallocate a 3D type(buf_intloc_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintloc_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintloc_3d(a)
          type(buf_intloc_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintloc_3d

!! \brief Deallocate a 1D type(buf_intlay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintlay_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintlay_1d(a)
          type(buf_intlay_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintlay_1d

!! \brief Deallocate a 2D type(buf_intlay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintlay_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintlay_2d(a)
          type(buf_intlay_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintlay_2d

!! \brief Deallocate a 3D type(buf_intlay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbufintlay_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbufintlay_3d(a)
          type(buf_intlay_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbufintlay_3d

!! \brief Deallocate a 1D type(buf_lay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuflay_1d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuflay_1d(a)
          type(buf_lay_), dimension(:), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuflay_1d

!! \brief Deallocate a 2D type(buf_lay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuflay_2d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuflay_2d(a)
          type(buf_lay_), dimension(:, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuflay_2d

!! \brief Deallocate a 3D type(buf_lay_) array (pointer)
!||====================================================================
!||    my_dealloc_pbuflay_3d   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine my_dealloc_pbuflay_3d(a)
          type(buf_lay_), dimension(:, :, :), pointer, intent(inout) :: a
          if (associated(a)) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(a(lbound(a,1), lbound(a,2), lbound(a,3))))
            deallocate(a)
            nullify(a)
          end if
        end subroutine my_dealloc_pbuflay_3d


! ======================================================================================================================
!                            PLACEHOLDER — DERIVED TYPE DEALLOCATION ROUTINES
!
! To add derived type support, add the type to the TYPES set above, or add a dedicated
! interface block below for types that require special teardown before dealloc.
! ======================================================================================================================

      end module my_dealloc_mod
