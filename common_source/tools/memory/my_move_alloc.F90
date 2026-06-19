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



!||====================================================================
!||    my_move_alloc_mod      ../common_source/tools/memory/my_move_alloc.F90
!||--- called by ------------------------------------------------------
!||    extend_array_mod       ../common_source/tools/memory/extend_array.F90
!||    get_list_remnode       ../starter/source/interfaces/inter3d1/get_list_remnode.F90
!||    i25sto                 ../engine/source/interfaces/intsort/i25sto.F
!||    inter_save_candidate   ../starter/source/interfaces/inter3d1/inter_save_candidate.F90
!||    my_alloc_mod           ../common_source/tools/memory/my_alloc.F90
!||    realloc_iddconnect     ../starter/source/spmd/node/ddtools.F
!||    remn_i2op              ../starter/source/interfaces/inter3d1/i7remnode.F
!||    remn_i2op_edg25        ../starter/source/interfaces/int25/i25remlin.F
!||    split_cfd_solide       ../starter/source/spmd/split_cfd_solide.F
!||    update_pon_shells      ../engine/source/engine/node_spliting/update_pon.F90
!||    upgrade_cand_opt       ../common_source/interf/upgrade_multimp.F
!||    upgrade_lcand_e2s      ../common_source/interf/upgrade_multimp.F
!||    upgrade_lcand_edg      ../common_source/interf/upgrade_multimp.F
!||    upgrade_multimp        ../common_source/interf/upgrade_multimp.F
!||--- uses       -----------------------------------------------------
!||====================================================================
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

        private :: my_move_alloc_real_1d
        private :: my_move_alloc_real_2d
        private :: my_move_alloc_real_3d
        private :: my_move_alloc_double_1d
        private :: my_move_alloc_double_2d
        private :: my_move_alloc_double_3d
        private :: my_move_alloc_integer_1d
        private :: my_move_alloc_integer_2d
        private :: my_move_alloc_integer_3d
        private :: my_move_alloc_logical_1d
        private :: my_move_alloc_logical_2d
        private :: my_move_alloc_logical_3d

        public :: my_move_alloc

        interface my_move_alloc
          module procedure my_move_alloc_real_1d
          module procedure my_move_alloc_real_2d
          module procedure my_move_alloc_real_3d
          module procedure my_move_alloc_double_1d
          module procedure my_move_alloc_double_2d
          module procedure my_move_alloc_double_3d
          module procedure my_move_alloc_integer_1d
          module procedure my_move_alloc_integer_2d
          module procedure my_move_alloc_integer_3d
          module procedure my_move_alloc_logical_1d
          module procedure my_move_alloc_logical_2d
          module procedure my_move_alloc_logical_3d
        end interface my_move_alloc

      contains

!||====================================================================
!||    is_addr_tracked            ../common_source/tools/memory/my_move_alloc.F90
!||--- called by ------------------------------------------------------
!||    my_move_alloc_double_1d    ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_double_2d    ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_double_3d    ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_integer_1d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_integer_2d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_integer_3d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_logical_1d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_logical_2d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_logical_3d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_real_1d      ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_real_2d      ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_real_3d      ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        logical function is_addr_tracked(addr)
          type(c_ptr), intent(in) :: addr
          is_addr_tracked = (cpp_is_alloc_tracked_addr(addr) /= 0_c_int)
        end function is_addr_tracked

!||====================================================================
!||    record_alloc_addr          ../common_source/tools/memory/my_move_alloc.F90
!||--- called by ------------------------------------------------------
!||    my_alloc_8_double_1d       ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_double_2d       ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_double_3d       ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_integer_1d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_integer_2d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_integer_3d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_logical_1d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_logical_2d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_logical_3d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_pdouble_1d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_pdouble_2d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_pdouble_3d      ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_pinteger_1d     ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_pinteger_2d     ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_pinteger_3d     ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_plogical_1d     ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_plogical_2d     ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_plogical_3d     ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_preal_1d        ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_preal_2d        ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_preal_3d        ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_real_1d         ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_real_2d         ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_8_real_3d         ../common_source/tools/memory/my_alloc_impl_idx8.F90
!||    my_alloc_bufdamp_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufeos_1d         ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_buffail_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufintlay_1d      ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufintloc_1d      ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_buflay_1d         ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufmat_1d         ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufnloc_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufnlocts_1d      ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufporo_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufprop_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufvisc_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_bufxfem_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_double_1d         ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_double_2d         ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_double_3d         ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_elbuf_1d          ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_failloc_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_gbuf_1d           ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_integer_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_integer_2d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_integer_3d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_lbuf_1d           ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_lbufdir_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_logical_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_logical_2d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_logical_3d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufdamp_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufeos_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbuffail_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufintlay_1d     ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufintloc_1d     ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbuflay_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufmat_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufnloc_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufnlocts_1d     ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufporo_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufprop_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufvisc_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pbufxfem_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pdouble_1d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pdouble_2d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pdouble_3d        ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pelbuf_1d         ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pfailloc_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pgbuf_1d          ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pinteger_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pinteger_2d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_pinteger_3d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_plbuf_1d          ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_plbufdir_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_plogical_1d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_plogical_2d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_plogical_3d       ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_preal_1d          ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_preal_2d          ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_preal_3d          ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_real_1d           ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_real_2d           ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_alloc_real_3d           ../common_source/tools/memory/my_alloc_impl_idx4.F90
!||    my_move_alloc_double_1d    ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_double_2d    ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_double_3d    ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_integer_1d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_integer_2d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_integer_3d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_logical_1d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_logical_2d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_logical_3d   ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_real_1d      ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_real_2d      ../common_source/tools/memory/my_move_alloc.F90
!||    my_move_alloc_real_3d      ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||====================================================================
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

!! \brief move_alloc wrapper for 1D real arrays with memory report update
!||====================================================================
!||    my_move_alloc_real_1d        ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_real_1d(from, to, msg)
          real, dimension(:), allocatable, target, intent(inout) :: from
          real, dimension(:), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_real_1d

!! \brief move_alloc wrapper for 2D real arrays with memory report update
!||====================================================================
!||    my_move_alloc_real_2d        ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_real_2d(from, to, msg)
          real, dimension(:, :), allocatable, target, intent(inout) :: from
          real, dimension(:, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_real_2d

!! \brief move_alloc wrapper for 3D real arrays with memory report update
!||====================================================================
!||    my_move_alloc_real_3d        ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_real_3d(from, to, msg)
          real, dimension(:, :, :), allocatable, target, intent(inout) :: from
          real, dimension(:, :, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2), lbound(from,3)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_real_3d

!! \brief move_alloc wrapper for 1D double precision arrays with memory report update
!||====================================================================
!||    my_move_alloc_double_1d      ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_double_1d(from, to, msg)
          double precision, dimension(:), allocatable, target, intent(inout) :: from
          double precision, dimension(:), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_double_1d

!! \brief move_alloc wrapper for 2D double precision arrays with memory report update
!||====================================================================
!||    my_move_alloc_double_2d      ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_double_2d(from, to, msg)
          double precision, dimension(:, :), allocatable, target, intent(inout) :: from
          double precision, dimension(:, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_double_2d

!! \brief move_alloc wrapper for 3D double precision arrays with memory report update
!||====================================================================
!||    my_move_alloc_double_3d      ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_double_3d(from, to, msg)
          double precision, dimension(:, :, :), allocatable, target, intent(inout) :: from
          double precision, dimension(:, :, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2), lbound(from,3)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_double_3d

!! \brief move_alloc wrapper for 1D integer arrays with memory report update
!||====================================================================
!||    my_move_alloc_integer_1d     ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_integer_1d(from, to, msg)
          integer, dimension(:), allocatable, target, intent(inout) :: from
          integer, dimension(:), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_integer_1d

!! \brief move_alloc wrapper for 2D integer arrays with memory report update
!||====================================================================
!||    my_move_alloc_integer_2d     ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_integer_2d(from, to, msg)
          integer, dimension(:, :), allocatable, target, intent(inout) :: from
          integer, dimension(:, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_integer_2d

!! \brief move_alloc wrapper for 3D integer arrays with memory report update
!||====================================================================
!||    my_move_alloc_integer_3d     ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_integer_3d(from, to, msg)
          integer, dimension(:, :, :), allocatable, target, intent(inout) :: from
          integer, dimension(:, :, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2), lbound(from,3)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_integer_3d

!! \brief move_alloc wrapper for 1D logical arrays with memory report update
!||====================================================================
!||    my_move_alloc_logical_1d     ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_logical_1d(from, to, msg)
          logical, dimension(:), allocatable, target, intent(inout) :: from
          logical, dimension(:), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_logical_1d

!! \brief move_alloc wrapper for 2D logical arrays with memory report update
!||====================================================================
!||    my_move_alloc_logical_2d     ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_logical_2d(from, to, msg)
          logical, dimension(:, :), allocatable, target, intent(inout) :: from
          logical, dimension(:, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_logical_2d

!! \brief move_alloc wrapper for 3D logical arrays with memory report update
!||====================================================================
!||    my_move_alloc_logical_3d     ../common_source/tools/memory/my_move_alloc.F90
!||--- calls      -----------------------------------------------------
!||    is_addr_tracked              ../common_source/tools/memory/my_move_alloc.F90
!||    record_alloc_addr            ../common_source/tools/memory/my_move_alloc.F90
!||====================================================================
        subroutine my_move_alloc_logical_3d(from, to, msg)
          logical, dimension(:, :, :), allocatable, target, intent(inout) :: from
          logical, dimension(:, :, :), allocatable, target, intent(inout) :: to
          character(len=*), optional, intent(in) :: msg

          type(c_ptr) :: from_addr, to_old_addr, to_new_addr
          logical :: from_live, to_old_live, to_new_live, from_tracked
          integer(kind=8) :: nbytes

          from_live = allocated(from) .and. size(from) > 0
          to_old_live = allocated(to) .and. size(to) > 0
          from_tracked = .false.

          if (from_live) then
            from_addr = c_loc(from(lbound(from,1), lbound(from,2), lbound(from,3)))
            from_tracked = is_addr_tracked(from_addr)
          end if

          if (to_old_live) then
            to_old_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            call cpp_record_dealloc_addr(to_old_addr)
          end if

          call move_alloc(from, to)

          to_new_live = allocated(to) .and. size(to) > 0
          if (to_new_live) then
            to_new_addr = c_loc(to(lbound(to,1), lbound(to,2), lbound(to,3)))
            if (from_tracked) then
              call cpp_record_move_alloc_addr(from_addr, to_new_addr)
            else if (present(msg)) then
              nbytes = int(storage_size(to), kind=8) / 8_8 * size(to, kind=8)
              call record_alloc_addr(to_new_addr, msg, nbytes)
            end if
          end if
        end subroutine my_move_alloc_logical_3d


      end module my_move_alloc_mod
