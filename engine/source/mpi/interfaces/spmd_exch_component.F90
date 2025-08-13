!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    spmd_exchange_component_mod   ../engine/source/mpi/interfaces/spmd_exch_component.F90
!||--- called by ------------------------------------------------------
!||    inter_prepare_sort            ../engine/source/interfaces/generic/inter_prepare_sort.F
!||====================================================================
      module spmd_exchange_component_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    spmd_exchange_component   ../engine/source/mpi/interfaces/spmd_exch_component.F90
!||--- called by ------------------------------------------------------
!||    inter_prepare_sort        ../engine/source/interfaces/generic/inter_prepare_sort.F
!||--- calls      -----------------------------------------------------
!||    spmd_waitany              ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod              ../common_source/modules/constant_mod.F
!||    inter_sorting_mod         ../engine/share/modules/inter_sorting_mod.F
!||    spmd_mod                  ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine spmd_exchange_component(mode,nspmd,component)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use inter_sorting_mod , only : component_
          use spmd_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer :: mode !< 1 --> isend/ircv, 2 --> wait + check intersections
          integer, intent(in) :: nspmd !< number of processors
          type(component_), intent(inout) :: component

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,ncomp,r_ncomp
          integer :: my_size,my_offset,my_index
          integer :: proc_id
          integer, parameter :: my_tag = 400001
          my_real, dimension(6) :: l_bound,r_bound
          integer :: status_mpi(mpi_status_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------



! ----------------------------------------------------------------------------------------------------------------------
          if(mode==1) then
            my_size = component%s_comp_nb + component%m_comp_nb
            allocate(component%s_buffer_comp(6*my_size))
            do ncomp=1,component%s_comp_nb
              component%s_buffer_comp((ncomp-1)*6+1:(ncomp-1)*6+6) = component%s_list(ncomp)%bound(1:6)
            enddo
            my_offset = 6*component%s_comp_nb
            do ncomp=1,component%m_comp_nb
              component%s_buffer_comp(my_offset+(ncomp-1)*6+1:my_offset+(ncomp-1)*6+6) = component%m_list(ncomp)%bound(1:6)
            enddo

            component%request_r_nb = 0
            if(.not.allocated(component%request_r)) then
              allocate(component%request_r(nspmd))
            endif
            if(.not.allocated(component%index_r)) then
              allocate(component%index_r(nspmd))
            endif
            do i=1,nspmd

              if(component%proc_comp(i)%need_comm0) then
                component%proc_comp(i)%remote_comp = -123456789.
                my_size = 6*(component%proc_comp(i)%remote_s_comp_nb+component%proc_comp(i)%remote_m_comp_nb)
                component%request_r_nb = component%request_r_nb + 1
                component%index_r(component%request_r_nb) = i
                call spmd_irecv(component%proc_comp(i)%remote_comp,my_size,i-1,my_tag, &
                  component%request_r(component%request_r_nb),SPMD_COMM_WORLD)
              endif
            enddo

            my_size = 6*(component%s_comp_nb+component%m_comp_nb)
            component%request_s_nb = 0
            if(.not.allocated(component%request_s)) then
              allocate(component%request_s(nspmd))
            endif
            do i=1,nspmd
              if(component%proc_comp(i)%need_comm0) then
                component%request_s_nb = component%request_s_nb + 1
                call spmd_isend(component%s_buffer_comp,my_size,i-1,my_tag, &
                  component%request_s(component%request_s_nb),SPMD_COMM_WORLD)
              endif
            enddo
          else
            do i=1,component%request_r_nb
              call spmd_waitany(component%request_r_nb,component%request_r,my_index,status_mpi)
              proc_id = component%index_r(my_index)
              component%proc_comp(proc_id)%need_comm_s = .false.
              component%proc_comp(proc_id)%need_comm_r = .false.

              my_offset = 6*component%proc_comp(proc_id)%remote_s_comp_nb
              do ncomp=1,component%s_comp_nb
                l_bound(1:6) = component%s_list(ncomp)%bound(1:6)
                ! check intersection between local S / remote M
                do r_ncomp=1,component%proc_comp(proc_id)%remote_m_comp_nb
                  r_bound(1:6) = component%proc_comp(proc_id)%remote_comp(my_offset+(r_ncomp-1)*6+1:my_offset+(r_ncomp-1)*6+6)
                  if( (((l_bound(1)>=r_bound(4)).and.(l_bound(1)<=r_bound(1))).or. &
                    ((l_bound(4)>=r_bound(4)).and.(l_bound(4)<=r_bound(1)))).or. &
                    (((r_bound(1)>=l_bound(4)).and.(r_bound(1)<=l_bound(1))).or. &
                    ((r_bound(4)>=l_bound(4)).and.(r_bound(4)<=l_bound(1))))   ) then

                    if( (((l_bound(2)>=r_bound(5)).and.(l_bound(2)<=r_bound(2))).or. &
                      ((l_bound(5)>=r_bound(5)).and.(l_bound(5)<=r_bound(2)))).or. &
                      (((r_bound(2)>=l_bound(5)).and.(r_bound(2)<=l_bound(2))).or. &
                      ((r_bound(5)>=l_bound(5)).and.(r_bound(5)<=l_bound(2))))  ) then

                      if( (((l_bound(3)>=r_bound(6)).and.(l_bound(3)<=r_bound(3))).or. &
                        ((l_bound(6)>=r_bound(6)).and.(l_bound(6)<=r_bound(3)))).or. &
                        (((r_bound(3)>=l_bound(6)).and.(r_bound(3)<=l_bound(3))).or. &
                        ((r_bound(6)>=l_bound(6)).and.(r_bound(6)<=l_bound(3)))) ) then
                        component%proc_comp(proc_id)%need_comm_r = .true.
                      endif
                    endif
                  endif
                enddo
              enddo

              my_offset = 0
              do ncomp=1,component%m_comp_nb
                l_bound(1:6) = component%m_list(ncomp)%bound(1:6)
                ! check intersection between local M / remote S

                do r_ncomp=1,component%proc_comp(proc_id)%remote_s_comp_nb
                  r_bound(1:6) = component%proc_comp(proc_id)%remote_comp((r_ncomp-1)*6+1:(r_ncomp-1)*6+6)

                  if( (((l_bound(1)>=r_bound(4)).and.(l_bound(1)<=r_bound(1))).or. &
                    ((l_bound(4)>=r_bound(4)).and.(l_bound(4)<=r_bound(1)))).or. &
                    (((r_bound(1)>=l_bound(4)).and.(r_bound(1)<=l_bound(1))).or. &
                    ((r_bound(4)>=l_bound(4)).and.(r_bound(4)<=l_bound(1))))   ) then

                    if( (((l_bound(2)>=r_bound(5)).and.(l_bound(2)<=r_bound(2))).or. &
                      ((l_bound(5)>=r_bound(5)).and.(l_bound(5)<=r_bound(2)))).or. &
                      (((r_bound(2)>=l_bound(5)).and.(r_bound(2)<=l_bound(2))).or. &
                      ((r_bound(5)>=l_bound(5)).and.(r_bound(5)<=l_bound(2))))  ) then

                      if( (((l_bound(3)>=r_bound(6)).and.(l_bound(3)<=r_bound(3))).or. &
                        ((l_bound(6)>=r_bound(6)).and.(l_bound(6)<=r_bound(3)))).or. &
                        (((r_bound(3)>=l_bound(6)).and.(r_bound(3)<=l_bound(3))).or. &
                        ((r_bound(6)>=l_bound(6)).and.(r_bound(6)<=l_bound(3)))) ) then
                        component%proc_comp(proc_id)%need_comm_s = .true.
                      endif
                    endif
                  endif
                enddo
              enddo
            enddo

            do i=1,component%request_s_nb
              call spmd_waitany(component%request_s_nb,component%request_s,my_index,status_mpi)
            enddo
            deallocate(component%s_buffer_comp)
          endif
        end subroutine spmd_exchange_component
      end module spmd_exchange_component_mod
