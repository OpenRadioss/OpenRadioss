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

#:set TYPES     = [('real', 'real'), ('double precision', 'double'), ('integer', 'integer'), ('logical', 'logical'), ('type(elbuf_struct_)', 'elbuf'), ('type(g_bufel_)', 'gbuf'), ('type(l_bufel_)', 'lbuf'), ('type(buf_prop_)', 'bufprop'), ('type(buf_nloc_)', 'bufnloc'), ('type(buf_nlocts_)', 'bufnlocts'), ('type(buf_damp_range_)', 'bufdamp'), ('type(buf_eos_)', 'bufeos'), ('type(buf_poro_)', 'bufporo'), ('type(buf_visc_)', 'bufvisc'), ('type(buf_xfem_)', 'bufxfem'), ('type(fail_loc_)', 'failloc'), ('type(buf_fail_)', 'buffail'), ('type(buf_mat_)', 'bufmat'), ('type(l_bufel_dir_)', 'lbufdir'), ('type(buf_intloc_)', 'bufintloc'), ('type(buf_intlay_)', 'bufintlay'), ('type(buf_lay_)', 'buflay')]
#:set MEM_KINDS = [('allocatable', ''), ('pointer', 'p')]
#:set RANKS     = [(1, ['n']), (2, ['n', 'm']), (3, ['l', 'm', 'n'])]
#:set ORDINALS  = ['first', 'second', 'third']

#! Macro: emit one allocation subroutine.
#! Arguments:
#!   SUB_NAME  - subroutine name string
#!   FTYPE     - Fortran type (e.g. 'real', 'double precision', 'integer', 'logical')
#!   RANK      - integer rank (1, 2, or 3)
#!   DIM_VARS  - list of dimension variable names  (e.g. ['n'] or ['n','m'] or ['l','m','n'])
#!   MEM_ATTR  - 'allocatable' or 'pointer'
#!   IDX_TYPE  - 'integer' or 'integer(8)'
#:def alloc_sub(SUB_NAME, FTYPE, RANK, DIM_VARS, MEM_ATTR, IDX_TYPE)
  #! Address of first element — used as unique tracking key per live allocation.
  #! TARGET attribute on the dummy allows c_loc on elements.
  #:set FIRST_ELEM = 'a(' + ', '.join(['lbound(a,' + str(i+1) + ')' for i in range(RANK)]) + ')'
!||====================================================================
!||    ${SUB_NAME}$
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc_tools.F90
!||====================================================================
        subroutine ${SUB_NAME}$(a, ${', '.join(DIM_VARS)}$, msg, stat, lower_bound)
        use iso_c_binding, only: c_loc
  #:if MEM_ATTR == 'allocatable'
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), allocatable, target, intent(inout) :: a !< The allocated array
  #:else
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), pointer, intent(inout) :: a !< The allocated array
  #:endif
  #:for I, DV in enumerate(DIM_VARS)
    #:if RANK == 1
          ${IDX_TYPE}$, intent(in) :: ${DV}$ !< The size of the array
    #:else
          ${IDX_TYPE}$, intent(in) :: ${DV}$ !< The ${ORDINALS[I]}$ dimension of the array
    #:endif
  #:endfor
          character(len=*), optional, intent(in)  :: msg         !< Error message printed on allocation failure
          integer,          optional, intent(out) :: stat        !< Allocation status code
          integer,          optional, intent(in)  :: lower_bound !< Lower bound for all dimensions (default: 1)
          integer :: ierr, lb_
          lb_ = 1
          if (present(lower_bound)) lb_ = lower_bound
          allocate(a(${', '.join([f'lb_:lb_+{dv}-1' for dv in DIM_VARS])}$), stat=ierr)
          if (ierr == 0 .and. present(msg) .and. size(a) > 0) &
            call record_alloc_addr(c_loc(${FIRST_ELEM}$), msg, &
                                   int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if (.not. present(stat)) then
            if (present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if (present(stat)) stat = ierr
        end subroutine ${SUB_NAME}$
#:enddef alloc_sub
