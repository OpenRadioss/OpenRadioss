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

#:set TYPES = [('real', 'real'), ('double precision', 'double'), ('integer', 'integer'), ('logical', 'logical'), ('type(elbuf_struct_)', 'elbuf'), ('type(g_bufel_)', 'gbuf'), ('type(l_bufel_)', 'lbuf'), ('type(buf_prop_)', 'bufprop'), ('type(buf_nloc_)', 'bufnloc'), ('type(buf_nlocts_)', 'bufnlocts'), ('type(buf_damp_range_)', 'bufdamp'), ('type(buf_eos_)', 'bufeos'), ('type(buf_poro_)', 'bufporo'), ('type(buf_visc_)', 'bufvisc'), ('type(buf_xfem_)', 'bufxfem'), ('type(fail_loc_)', 'failloc'), ('type(buf_fail_)', 'buffail'), ('type(buf_mat_)', 'bufmat'), ('type(l_bufel_dir_)', 'lbufdir'), ('type(buf_intloc_)', 'bufintloc'), ('type(buf_intlay_)', 'bufintlay'), ('type(buf_lay_)', 'buflay')]
#:set MEM_KINDS = [('allocatable', ''), ('pointer', 'p')]
#:set RANKS     = [(1, ['n']), (2, ['n', 'm']), (3, ['l', 'm', 'n'])]

#! Macro: emit one deallocation subroutine.
#! Arguments:
#!   SUB_NAME  - subroutine name string
#!   FTYPE     - Fortran type (e.g. 'real', 'double precision', 'integer', 'logical')
#!   RANK      - integer rank (1, 2, or 3)
#!   MEM_ATTR  - 'allocatable' or 'pointer'
#:def dealloc_sub(SUB_NAME, FTYPE, RANK, MEM_ATTR)
  #! Address expression: c_loc of the first element (TARGET required; pointers are
  #! implicit targets, allocatables get TARGET from the dummy declaration).
  #:set FIRST_ELEM = 'a(' + ', '.join(['lbound(a,' + str(i+1) + ')' for i in range(RANK)]) + ')'
  #:if MEM_ATTR == 'allocatable'
    #:set GUARD   = 'allocated(a)'
  #:else
    #:set GUARD   = 'associated(a)'
  #:endif
!||====================================================================
!||    ${SUB_NAME}$   ../common_source/tools/memory/my_dealloc.F90
!||--- calls      -----------------------------------------------------
!||    record_dealloc_addr   ../common_source/tools/memory/my_dealloc.F90
!||====================================================================
        subroutine ${SUB_NAME}$(a)
  #:if MEM_ATTR == 'allocatable'
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), allocatable, target, intent(inout) :: a
  #:else
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), pointer, intent(inout) :: a
  #:endif
          if (${GUARD}$) then
            if (size(a) > 0) call record_dealloc_addr(c_loc(${FIRST_ELEM}$))
            deallocate(a)
  #:if MEM_ATTR == 'pointer'
            nullify(a)
  #:endif
          end if
        end subroutine ${SUB_NAME}$
#:enddef dealloc_sub

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

#:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
  #:for FTYPE, TNAME in TYPES
    #:for RANK, DIM_VARS in RANKS
      #:if not FTYPE.startswith('type(') or MEM_ATTR == 'pointer'
        private :: my_dealloc_${MEM_PREFIX}$${TNAME}$_${RANK}$d
      #:endif
    #:endfor
  #:endfor
#:endfor

        public :: my_dealloc

        interface my_dealloc
#:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
  #:for FTYPE, TNAME in TYPES
    #:for RANK, DIM_VARS in RANKS
      #:if not FTYPE.startswith('type(') or MEM_ATTR == 'pointer'
          module procedure my_dealloc_${MEM_PREFIX}$${TNAME}$_${RANK}$d
      #:endif
    #:endfor
  #:endfor
#:endfor
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

#:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
  #:for FTYPE, TNAME in TYPES
    #:for RANK, DIM_VARS in RANKS
      #:if not FTYPE.startswith('type(') or MEM_ATTR == 'pointer'
        #:set SUB_NAME = 'my_dealloc_' + MEM_PREFIX + TNAME + '_' + str(RANK) + 'd'
!! \brief Deallocate a ${RANK}$D ${FTYPE}$ array (${MEM_ATTR}$)
$:dealloc_sub(SUB_NAME, FTYPE, RANK, MEM_ATTR)

      #:endif
    #:endfor
  #:endfor
#:endfor

! ======================================================================================================================
!                            PLACEHOLDER — DERIVED TYPE DEALLOCATION ROUTINES
!
! To add derived type support, add the type to the TYPES set above, or add a dedicated
! interface block below for types that require special teardown before dealloc.
! ======================================================================================================================

      end module my_dealloc_mod
