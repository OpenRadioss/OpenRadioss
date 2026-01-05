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
!||    inter_init_component_list_mod   ../engine/source/interfaces/generic/inter_init_component_list.F90
!||--- called by ------------------------------------------------------
!||    inter_init_component            ../engine/source/interfaces/generic/inter_init_component.F90
!||====================================================================
      module inter_init_component_list_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    inter_init_component_list   ../engine/source/interfaces/generic/inter_init_component_list.F90
!||--- called by ------------------------------------------------------
!||    inter_init_component        ../engine/source/interfaces/generic/inter_init_component.F90
!||--- uses       -----------------------------------------------------
!||    inter_sorting_mod           ../engine/share/modules/inter_sorting_mod.F
!||====================================================================
        subroutine inter_init_component_list( nsn,nrtm,numnod,nsv,irectm,s_node_color,m_node_color, &
          component )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use inter_sorting_mod , only : component_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nsn !< number of S nodes
          integer, intent(in) :: nrtm !< number of segment
          integer, intent(in) :: numnod !< number of nodes
          integer, dimension(nsn), intent(in) :: nsv !< list of S node id
          integer, dimension(4*nrtm), intent(in) :: irectm !< list of node id
          integer, dimension(nsn), intent(inout) :: s_node_color !<
          integer, dimension(nrtm), intent(inout) :: m_node_color !<
          type(component_), intent(inout) :: component !< component structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical, dimension(:), allocatable :: not_yet
          integer :: i,j
          integer :: my_color,node_nb,my_size,node_id
          integer, dimension(:), allocatable :: sm_node_nb,seg_nb
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          allocate(component%s_list(component%s_comp_nb))
          allocate(component%m_list(component%m_comp_nb))

          allocate(sm_node_nb(component%s_comp_nb))
          sm_node_nb(1:component%s_comp_nb) = 0
          do i=1,nsn
            my_color = s_node_color(i)
            sm_node_nb(my_color) = sm_node_nb(my_color) + 1
          end do

          do i=1,component%s_comp_nb
            allocate(component%s_list(i)%node(sm_node_nb(i)))
            allocate(component%s_list(i)%bound(6))
          end do
          sm_node_nb(1:component%s_comp_nb) = 0
          do i=1,nsn
            my_color = s_node_color(i)
            sm_node_nb(my_color) = sm_node_nb(my_color) + 1
            component%s_list(my_color)%node(sm_node_nb(my_color)) = nsv(i)
          end do

          my_size = 0
          do i=1,component%s_comp_nb
            my_size = my_size + sm_node_nb(i)
            component%s_list(i)%node_nb = sm_node_nb(i)
          end do

          deallocate(sm_node_nb)

          allocate(seg_nb(component%m_comp_nb))
          allocate(sm_node_nb(component%m_comp_nb))
          seg_nb(1:component%m_comp_nb) = 0
          sm_node_nb(1:component%m_comp_nb) = 0
          do i=1,nrtm
            my_color = m_node_color(i)
            seg_nb(my_color) = seg_nb(my_color) + 1
          end do
          do i=1,component%m_comp_nb
            allocate(component%m_list(i)%seg(seg_nb(i)))
            allocate(component%m_list(i)%node(4*seg_nb(i)))
            allocate(component%m_list(i)%bound(6))
            component%m_list(i)%node_nb = 0
          end do
          seg_nb(1:component%m_comp_nb) = 0
          allocate(not_yet(numnod))
          not_yet(:) = .true.
          do i=1,nrtm
            my_color = m_node_color(i)
            seg_nb(my_color) = seg_nb(my_color) + 1
            component%m_list(my_color)%seg(seg_nb(my_color)) = i
            node_nb = 3
            if(irectm((i-1)*4+3)/=irectm((i-1)*4+4)) node_nb = 4
            do j=1,node_nb
              node_id = irectm((i-1)*4+j)
              sm_node_nb(my_color) = sm_node_nb(my_color) + 1
              component%m_list(my_color)%node(sm_node_nb(my_color)) = irectm((i-1)*4+j)
            end do
          end do
          deallocate(not_yet)
          my_size = 0
          do i=1,component%m_comp_nb
            my_size = my_size + sm_node_nb(i)
            component%m_list(i)%node_nb = sm_node_nb(i)
          end do
          deallocate(seg_nb)
          deallocate(sm_node_nb)

        end subroutine inter_init_component_list
      end module inter_init_component_list_mod
