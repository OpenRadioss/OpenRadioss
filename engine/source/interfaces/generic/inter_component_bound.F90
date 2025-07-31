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
!||    inter_component_bound_mod   ../engine/source/interfaces/generic/inter_component_bound.F90
!||--- called by ------------------------------------------------------
!||    inter_prepare_sort          ../engine/source/interfaces/generic/inter_prepare_sort.F
!||====================================================================
      module inter_component_bound_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    inter_component_bound   ../engine/source/interfaces/generic/inter_component_bound.F90
!||--- called by ------------------------------------------------------
!||    inter_prepare_sort      ../engine/source/interfaces/generic/inter_prepare_sort.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    inter_sorting_mod       ../engine/share/modules/inter_sorting_mod.F
!||====================================================================
        subroutine inter_component_bound(numnod,tzinf,curv,x,component)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use constant_mod
        use inter_sorting_mod , only : component_
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
        integer, intent(in) :: numnod !< number of nodes
        my_real, intent(in) :: tzinf !< max tzinf
        my_real, intent(in) :: curv !< max curv
        my_real, dimension(3,numnod), intent(in) :: x !< node coordinates
        type(component_), intent(inout) :: component

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer :: i,ncomp
        integer :: node_id
        my_real, dimension(6) :: local_bound
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        
        !$omp single          
        do ncomp=1,component%s_comp_nb
          component%s_list(ncomp)%bound(1:3) = -ep30
          component%s_list(ncomp)%bound(4:6) = ep30
        enddo
        do ncomp=1,component%m_comp_nb
          component%m_list(ncomp)%bound(1:3) = -ep30
          component%m_list(ncomp)%bound(4:6) = ep30
        enddo
        !$omp end single     
                
        do ncomp=1,component%s_comp_nb
          local_bound(1:3) = -ep30
          local_bound(4:6) = ep30
          !$omp do schedule(dynamic)
          do i=1,component%s_list(ncomp)%node_nb
            node_id = component%s_list(ncomp)%node(i)
            local_bound(1) = max(local_bound(1),x(1,node_id))
            local_bound(2) = max(local_bound(2),x(2,node_id))
            local_bound(3) = max(local_bound(3),x(3,node_id))
            local_bound(4) = min(local_bound(4),x(1,node_id))
            local_bound(5) = min(local_bound(5),x(2,node_id))
            local_bound(6) = min(local_bound(6),x(3,node_id))
          enddo
          !$omp end do

          !$omp critical
          component%s_list(ncomp)%bound(1) = max(component%s_list(ncomp)%bound(1),local_bound(1))
          component%s_list(ncomp)%bound(2) = max(component%s_list(ncomp)%bound(2),local_bound(2))
          component%s_list(ncomp)%bound(3) = max(component%s_list(ncomp)%bound(3),local_bound(3))
          component%s_list(ncomp)%bound(4) = min(component%s_list(ncomp)%bound(4),local_bound(4))
          component%s_list(ncomp)%bound(5) = min(component%s_list(ncomp)%bound(5),local_bound(5))
          component%s_list(ncomp)%bound(6) = min(component%s_list(ncomp)%bound(6),local_bound(6))
          !$omp end critical
        enddo

        do ncomp=1,component%m_comp_nb
          local_bound(1:3) = -ep30
          local_bound(4:6) = ep30
          !$omp do schedule(dynamic)
          do i=1,component%m_list(ncomp)%node_nb
            node_id = component%m_list(ncomp)%node(i)
            local_bound(1) = max(local_bound(1),x(1,node_id))
            local_bound(2) = max(local_bound(2),x(2,node_id))
            local_bound(3) = max(local_bound(3),x(3,node_id))
            local_bound(4) = min(local_bound(4),x(1,node_id))
            local_bound(5) = min(local_bound(5),x(2,node_id))
            local_bound(6) = min(local_bound(6),x(3,node_id))
          enddo
          !$omp end do
          
          !$omp critical          
          component%m_list(ncomp)%bound(1) = max(component%m_list(ncomp)%bound(1),local_bound(1))
          component%m_list(ncomp)%bound(2) = max(component%m_list(ncomp)%bound(2),local_bound(2))
          component%m_list(ncomp)%bound(3) = max(component%m_list(ncomp)%bound(3),local_bound(3))
          component%m_list(ncomp)%bound(4) = min(component%m_list(ncomp)%bound(4),local_bound(4))
          component%m_list(ncomp)%bound(5) = min(component%m_list(ncomp)%bound(5),local_bound(5))
          component%m_list(ncomp)%bound(6) = min(component%m_list(ncomp)%bound(6),local_bound(6))            
          !$omp end critical
        enddo    

        !$omp barrier

        !$omp single          
        do ncomp=1,component%m_comp_nb
          component%m_list(ncomp)%bound(1) = component%m_list(ncomp)%bound(1) + tzinf + curv
          component%m_list(ncomp)%bound(2) = component%m_list(ncomp)%bound(2) + tzinf + curv
          component%m_list(ncomp)%bound(3) = component%m_list(ncomp)%bound(3) + tzinf + curv
          component%m_list(ncomp)%bound(4) = component%m_list(ncomp)%bound(4) - tzinf - curv
          component%m_list(ncomp)%bound(5) = component%m_list(ncomp)%bound(5) - tzinf - curv
          component%m_list(ncomp)%bound(6) = component%m_list(ncomp)%bound(6) - tzinf - curv
        enddo
        !$omp end single     

        return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine inter_component_bound
      end module inter_component_bound_mod