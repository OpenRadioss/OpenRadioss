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
!||    inter_init_node_color_mod   ../engine/source/interfaces/generic/inter_init_node_color.F90
!||--- called by ------------------------------------------------------
!||    inter_init_component        ../engine/source/interfaces/generic/inter_init_component.F90
!||====================================================================
      module inter_init_node_color_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
!||====================================================================
!||    inter_init_node_color   ../engine/source/interfaces/generic/inter_init_node_color.F90
!||--- called by ------------------------------------------------------
!||    inter_init_component    ../engine/source/interfaces/generic/inter_init_component.F90
!||--- calls      -----------------------------------------------------
!||    alloc_int_1d_array      ../common_source/modules/array_mod.F
!||--- uses       -----------------------------------------------------
!||    array_mod               ../common_source/modules/array_mod.F
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine inter_init_node_color( nsn,nrtm,nb_cell_x,nb_cell_y,nb_cell_z, &
          numnod,s_comp_nb,m_comp_nb, &
          nsv,irectm,s_node_color,m_node_color, &
          box_limit,x)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use array_mod
          use constant_mod
          use precision_mod , only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "task_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nsn !< number of S nodes
          integer, intent(in) :: nrtm !< number of segment
          integer, intent(in) :: nb_cell_x !< number of cells in the x direction
          integer, intent(in) :: nb_cell_y !< number of cells in the y direction
          integer, intent(in) :: nb_cell_z !< number of cells in the z direction
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(inout) :: s_comp_nb !< number of S components
          integer, intent(inout) :: m_comp_nb !< number of M components
          integer, dimension(nsn), intent(in) :: nsv !< list of S node id
          integer, dimension(4*nrtm), intent(in) :: irectm !< list of M node id
          integer, dimension(nsn), intent(inout) :: s_node_color !<
          integer, dimension(nrtm), intent(inout) :: m_node_color !<
          real(kind=WP), dimension(6), intent(in) :: box_limit !< min and max position
          real(kind=WP), dimension(3,numnod), intent(in) :: x !< node coordinates
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: break_,break2_,new_neighbour
          integer :: my_position,my_neighbour
          integer :: next,next2
          integer :: s_cell_nb,m_cell_nb
          integer :: my_color,my_size
          integer :: i,j,k,ijk
          integer :: node_id,node_nb
          integer, dimension(3) :: cell_id,low_bound,up_bound
          real(kind=WP), dimension(3) :: barycentre
          integer, dimension(:), allocatable :: tmp_list,tmp_list2,s_color,m_color
          integer, dimension(:,:), allocatable :: s_index,m_index
          type(array_type_int_1d), dimension(:), allocatable :: s_connect,m_connect
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          my_size = nb_cell_x*nb_cell_y*nb_cell_z
          allocate(s_color(my_size))
          allocate(m_color(my_size))
          allocate(tmp_list(my_size))
          allocate(tmp_list2(my_size))
          allocate(s_index(my_size,4))
          allocate(m_index(my_size,4))
          allocate(s_connect(my_size))
          allocate(m_connect(my_size))

          s_index(1:my_size,1:4) = 0
          m_index(1:my_size,1:4) = 0
          do i=1,my_size
            s_connect(i)%size_int_array_1d = 0
            m_connect(i)%size_int_array_1d = 0
          enddo
          s_color(1:my_size) = 0
          m_color(1:my_size) = 0

          s_cell_nb = 0
          do i=1,nsn
            node_id = nsv(i)
            cell_id(1) = int(nb_cell_x*(x(1,node_id)-box_limit(4))/(box_limit(1)-box_limit(4)))
            cell_id(2) = int(nb_cell_y*(x(2,node_id)-box_limit(5))/(box_limit(2)-box_limit(5)))
            cell_id(3) = int(nb_cell_z*(x(3,node_id)-box_limit(6))/(box_limit(3)-box_limit(6)))

            cell_id(1) = max(1,min(nb_cell_x,cell_id(1)))
            cell_id(2) = max(1,min(nb_cell_y,cell_id(2)))
            cell_id(3) = max(1,min(nb_cell_z,cell_id(3)))

            my_position = cell_id(1) + (cell_id(2)-1)*nb_cell_x + (cell_id(3)-1)*nb_cell_x*nb_cell_y
            s_node_color(i) = -my_position
            if(s_color(my_position)==0) then
              s_color(my_position) = -1
              s_cell_nb = s_cell_nb + 1
              s_index(s_cell_nb,1) = my_position
              s_index(s_cell_nb,2) = cell_id(1)
              s_index(s_cell_nb,3) = cell_id(2)
              s_index(s_cell_nb,4) = cell_id(3)
            endif
          enddo

          do ijk=1,s_cell_nb
            my_position = s_index(ijk,1)
            i = s_index(ijk,2)
            j = s_index(ijk,3)
            k = s_index(ijk,4)
            low_bound(3) = k
            if(k>1) low_bound(3) = k-1

            up_bound(3) = k
            if(k<nb_cell_z) up_bound(3) = k+1

            low_bound(2) = j
            if(j>1) low_bound(2) = j-1

            up_bound(2) = j
            if(j<nb_cell_y) up_bound(2) = j+1

            low_bound(1) = i
            if(i>1) low_bound(1) = i-1

            up_bound(1) = i
            if(i<nb_cell_x) up_bound(1) = i+1

            next = 0
            do k=low_bound(3),up_bound(3)
              do j=low_bound(2),up_bound(2)
                do i=low_bound(1),up_bound(1)
                  my_neighbour = i + (j-1)*nb_cell_x + (k-1)*nb_cell_x*nb_cell_y
                  if(s_color(my_neighbour)/=0) then
                    next = next + 1
                    tmp_list(next) = my_neighbour
                  endif
                enddo
              enddo
            enddo

            s_connect(my_position)%size_int_array_1d = next
            call alloc_int_1d_array(s_connect(my_position))
            s_connect(my_position)%int_array_1d(1:next) = tmp_list(1:next)
          enddo

          my_color = 0
          if(s_cell_nb>0) then
            my_color = 1
            break_ = .true.
            tmp_list2(1) = s_index(1,1)
            next2 = 1
            s_color(tmp_list2(1)) = my_color
            do while(break_)
              tmp_list(1:next2) = tmp_list2(1:next2)
              next = next2
              next2 = 0
              new_neighbour = .false.
              do i=1,next
                my_position = tmp_list(i)

                if(s_color(my_position)==-1.or.s_color(my_position)==my_color) then
                  s_color(my_position) = my_color
                endif

                do j=1,s_connect(my_position)%size_int_array_1d
                  my_neighbour = s_connect(my_position)%int_array_1d(j)
                  if(s_color(my_neighbour)==-1) then
                    new_neighbour = .true.
                    next2 = next2 + 1
                    tmp_list2(next2) = my_neighbour
                    s_color(my_neighbour) = my_color
                  endif
                enddo
              enddo
              if(.not.new_neighbour) then
                break_ = .false.
                break2_ = .true.
                do ijk=1,s_cell_nb
                  my_position = s_index(ijk,1)
                  if(break2_) then
                    if(s_color(my_position)==-1) then
                      my_color = my_color + 1
                      next2 = 1
                      tmp_list2(1) = my_position
                      s_color(my_position) = my_color
                      break_ = .true.
                      break2_ = .false.
                    endif
                  endif
                enddo
              endif
            enddo
          endif

          s_comp_nb = my_color

          do i=1,nsn
            my_position = abs(s_node_color(i))
            s_node_color(i) = s_color(my_position)
          enddo



          m_cell_nb = 0
          do i=1,nrtm
            node_nb = 3
            if(irectm((i-1)*4+3)/=irectm((i-1)*4+4)) node_nb = 4
            barycentre(1:3) = zero
            do j=1,node_nb
              node_id = irectm((i-1)*4+j)
              barycentre(1:3) = barycentre(1:3) + x(1:3,node_id)
            enddo
            barycentre(1:3) = barycentre(1:3) / node_nb
            cell_id(1) = int(nb_cell_x*(barycentre(1)-box_limit(4))/(box_limit(1)-box_limit(4)))
            cell_id(2) = int(nb_cell_y*(barycentre(2)-box_limit(5))/(box_limit(2)-box_limit(5)))
            cell_id(3) = int(nb_cell_z*(barycentre(3)-box_limit(6))/(box_limit(3)-box_limit(6)))

            cell_id(1) = max(1,min(nb_cell_x,cell_id(1)))
            cell_id(2) = max(1,min(nb_cell_y,cell_id(2)))
            cell_id(3) = max(1,min(nb_cell_z,cell_id(3)))
            my_position = cell_id(1) + (cell_id(2)-1)*nb_cell_x + (cell_id(3)-1)*nb_cell_x*nb_cell_y
            m_node_color(i) = -my_position
            if(m_color(my_position)==0) then
              m_color(my_position) = -1
              m_cell_nb = m_cell_nb + 1
              m_index(m_cell_nb,1) = my_position
              m_index(m_cell_nb,2) = cell_id(1)
              m_index(m_cell_nb,3) = cell_id(2)
              m_index(m_cell_nb,4) = cell_id(3)
            endif
          enddo

          do ijk=1,m_cell_nb
            my_position = m_index(ijk,1)
            i = m_index(ijk,2)
            j = m_index(ijk,3)
            k = m_index(ijk,4)

            low_bound(1) = i
            if(i>1) low_bound(1) = i-1

            up_bound(1) = i
            if(i<nb_cell_x) up_bound(1) = i+1

            low_bound(2) = j
            if(j>1) low_bound(2) = j-1

            up_bound(2) = j
            if(j<nb_cell_y) up_bound(2) = j+1

            low_bound(3) = k
            if(k>1) low_bound(3) = k-1

            up_bound(3) = k
            if(k<nb_cell_z) up_bound(3) = k+1

            next = 0
            do k=low_bound(3),up_bound(3)
              do j=low_bound(2),up_bound(2)
                do i=low_bound(1),up_bound(1)
                  my_neighbour = i + (j-1)*nb_cell_x + (k-1)*nb_cell_x*nb_cell_y
                  if(m_color(my_neighbour)/=0) then
                    next = next + 1
                    tmp_list(next) = my_neighbour
                  endif
                enddo
              enddo
            enddo

            m_connect(my_position)%size_int_array_1d = next
            call alloc_int_1d_array(m_connect(my_position))
            m_connect(my_position)%int_array_1d(1:next) = tmp_list(1:next)
          enddo

          my_color = 0
          if(m_cell_nb>0) then
            my_color = 1
            break_ = .true.
            tmp_list2(1) = m_index(1,1)
            next2 = 1
            m_color(tmp_list2(1)) = my_color
            do while(break_)
              tmp_list(1:next2) = tmp_list2(1:next2)
              next = next2
              next2 = 0
              new_neighbour = .false.
              do i=1,next
                my_position = tmp_list(i)
                if(m_color(my_position)==-1.or.m_color(my_position)==my_color) then
                  m_color(my_position) = my_color
                endif
                do j=1,m_connect(my_position)%size_int_array_1d
                  my_neighbour = m_connect(my_position)%int_array_1d(j)
                  if(m_color(my_neighbour)==-1) then
                    new_neighbour = .true.
                    next2 = next2 + 1
                    tmp_list2(next2) = my_neighbour
                    m_color(my_neighbour) = my_color
                  endif
                enddo
              enddo
              if(.not.new_neighbour) then
                break_ = .false.
                break2_ = .true.
                do ijk=1,m_cell_nb
                  my_position = m_index(ijk,1)
                  if(break2_) then
                    if(m_color(my_position)==-1) then
                      my_color = my_color + 1
                      next2 = 1
                      tmp_list2(1) = my_position
                      m_color(my_position) = my_color
                      break_ = .true.
                      break2_ = .false.
                    endif
                  endif
                enddo
              endif
            enddo
          endif

          m_comp_nb = my_color
          do i=1,nrtm
            my_position = abs(m_node_color(i))
            m_node_color(i) = m_color(my_position)
          enddo


          deallocate(s_color)
          deallocate(m_color)
          deallocate(tmp_list)
          deallocate(tmp_list2)
          deallocate(s_connect)
          deallocate(m_connect)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine inter_init_node_color
      end module inter_init_node_color_mod
