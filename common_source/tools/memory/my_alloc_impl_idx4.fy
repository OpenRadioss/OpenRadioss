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

#:set IDX_KINDS = [('integer', '')]
#:include 'my_alloc_impl_core.fy'

      module my_alloc_impl_idx4_mod
        use iso_c_binding,      only : c_loc
        use my_alloc_tools_mod, only : my_alloc_check, record_alloc_addr
        use elbufdef_mod, only : elbuf_struct_, g_bufel_, l_bufel_, buf_prop_, buf_nloc_, &
                                  buf_nlocts_, buf_damp_range_, buf_eos_, buf_poro_, buf_visc_, buf_xfem_, &
                                  fail_loc_, buf_fail_, buf_mat_, l_bufel_dir_, buf_intloc_, buf_intlay_, buf_lay_
        implicit none

#:for IDX_TYPE, IDX_PREFIX in IDX_KINDS
  #:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
    #:for FTYPE, TNAME in TYPES
      #:for RANK, DIM_VARS in RANKS
        #:if RANK == 1 or not FTYPE.startswith('type(')
        private :: my_alloc_${IDX_PREFIX}$${MEM_PREFIX}$${TNAME}$_${RANK}$d
        #:endif
      #:endfor
    #:endfor
  #:endfor
#:endfor

        public :: my_alloc

        interface my_alloc
#:for IDX_TYPE, IDX_PREFIX in IDX_KINDS
  #:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
    #:for FTYPE, TNAME in TYPES
      #:for RANK, DIM_VARS in RANKS
        #:if RANK == 1 or not FTYPE.startswith('type(')
          module procedure my_alloc_${IDX_PREFIX}$${MEM_PREFIX}$${TNAME}$_${RANK}$d
        #:endif
      #:endfor
    #:endfor
  #:endfor
#:endfor
        end interface my_alloc

      contains

! ======================================================================================================================
!                          GENERATED ALLOCATION ROUTINES  (integer(4) dimension arguments)
!   Loop order: MEM_KINDS x TYPES x RANKS
! ======================================================================================================================

#:for IDX_TYPE, IDX_PREFIX in IDX_KINDS
  #:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
    #:for FTYPE, TNAME in TYPES
      #:for RANK, DIM_VARS in RANKS
        #:if RANK == 1 or not FTYPE.startswith('type(')
          #:set SUB_NAME = 'my_alloc_' + IDX_PREFIX + MEM_PREFIX + TNAME + '_' + str(RANK) + 'd'
!! \brief Allocate a ${RANK}$D ${FTYPE}$ array (${MEM_ATTR}$, ${IDX_TYPE}$ dims)
$:alloc_sub(SUB_NAME, FTYPE, RANK, DIM_VARS, MEM_ATTR, IDX_TYPE)

        #:endif
      #:endfor
    #:endfor
  #:endfor
#:endfor

      end module my_alloc_impl_idx4_mod
