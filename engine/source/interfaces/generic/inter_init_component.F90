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
      !||    inter_init_component_mod   ../engine/source/interfaces/generic/inter_init_component.F90
      !||--- called by ------------------------------------------------------
      !||    resol                      ../engine/source/engine/resol.F
      !||====================================================================
      module inter_init_component_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      !||====================================================================
      !||    inter_init_component            ../engine/source/interfaces/generic/inter_init_component.F90
      !||--- called by ------------------------------------------------------
      !||    resol                           ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    inter_box_creation              ../engine/source/interfaces/generic/inter_box_creation.F
      !||    inter_init_component_list       ../engine/source/interfaces/generic/inter_init_component_list.F90
      !||    inter_init_node_color           ../engine/source/interfaces/generic/inter_init_node_color.F90
      !||    inter_minmax_node               ../engine/source/interfaces/generic/inter_minmax_node.F
      !||    omp_get_thread_num              ../engine/source/engine/openmp_stub.F90
      !||    spmd_box_limit_reduction        ../engine/source/mpi/interfaces/spmd_box_limit_reduction.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                    ../common_source/modules/constant_mod.F
      !||    intbufdef_mod                   ../common_source/modules/interfaces/intbufdef_mod.F90
      !||    inter_init_component_list_mod   ../engine/source/interfaces/generic/inter_init_component_list.F90
      !||    inter_init_node_color_mod       ../engine/source/interfaces/generic/inter_init_node_color.F90
      !||    inter_sorting_mod               ../engine/share/modules/inter_sorting_mod.F
      !||    spmd_mod                        ../engine/source/mpi/spmd_mod.F90
      !||====================================================================
        subroutine inter_init_component(ninter,npari,numnod,ispmd,nspmd, &
                                        ipari,x,intbuf_tab,component)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use intbufdef_mod
        use constant_mod
        use inter_init_node_color_mod , only : inter_init_node_color
        use inter_init_component_list_mod , only : inter_init_component_list
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
        integer, intent(in) :: ninter !< number of interfaces
        integer, intent(in) :: npari !< number of interface parameters
        integer, intent(in) :: numnod !< number of nodes
        integer, intent(in) :: ispmd !< processor id
        integer, intent(in) :: nspmd !< number of processors
        integer, dimension(npari,ninter), intent(in) :: ipari !< interface parameters
        my_real, dimension(3,numnod), intent(in) :: x !< node coordinates
        type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab !< interface structure
        type(component_), dimension(ninter), intent(inout) :: component

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        logical :: type18
        integer :: i,j,k
        integer :: nsn,nmn,nty,nrtm
        integer :: nb_cell_x,nb_cell_y,nb_cell_z        
        integer :: type7_nb,inacti,my_size
        integer :: itask
        integer, dimension(ninter) :: type7_list
        my_real, dimension(6) :: box_limit
        integer, dimension(:), allocatable :: s_node_color,m_node_color
        integer, dimension(:), allocatable :: s_buffer,r_buffer
        integer, parameter :: my_tag = 400000
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
        integer, external :: omp_get_thread_num
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        box_limit(1:3) = -ep30
        box_limit(4:6) = ep30
        type7_nb = 0
        do i=1,ninter
          component(i)%s_comp_nb = 0
          component(i)%m_comp_nb = 0
          component(i)%total_m_remote_comp_nb = 0
          component(i)%total_s_remote_comp_nb = 0
          nty = ipari(7,i)
          inacti = ipari(22,i)
          type18 = .false.
          if(nty==7.and.inacti==7) type18 = .true.
          if(nty==7.and..not.type18) then
            type7_nb = type7_nb + 1
            type7_list(type7_nb) = i
!$OMP PARALLEL PRIVATE(itask)
            itask = omp_get_thread_num()
            call inter_minmax_node(itask,box_limit,x)
!$OMP END PARALLEL            
          endif
        enddo

        call spmd_box_limit_reduction(type7_nb,box_limit)

        nb_cell_x = 0
        nb_cell_y = 0
        nb_cell_z = 0
        call inter_box_creation(nb_cell_x,nb_cell_y,nb_cell_z,box_limit)
        do j=1,type7_nb
          i = type7_list(j)
          nrtm = ipari(4,i)
          nsn = ipari(5,i)
          nmn = ipari(6,i)          
          allocate(s_node_color(nsn))
          allocate(m_node_color(nrtm))
          call inter_init_node_color( nsn,nrtm,nb_cell_x,nb_cell_y,nb_cell_z, &
                                      numnod,component(i)%s_comp_nb,component(i)%m_comp_nb, &
                                      intbuf_tab(i)%nsv,intbuf_tab(i)%irectm,s_node_color,m_node_color, &
                                      box_limit,x)
          call inter_init_component_list( nsn,nrtm,numnod,intbuf_tab(i)%nsv,intbuf_tab(i)%irectm,s_node_color,m_node_color, &
                                          component(i) )                      
          allocate(component(i)%proc_comp(nspmd))          
          component(i)%proc_comp(1:nspmd)%remote_m_comp_nb = 0
          component(i)%proc_comp(1:nspmd)%remote_s_comp_nb = 0
          component(i)%proc_comp(1:nspmd)%need_comm0 = .false.
          component(i)%proc_comp(1:nspmd)%need_comm_s = .false.
          component(i)%proc_comp(1:nspmd)%need_comm_r = .false.

          deallocate(s_node_color)
          deallocate(m_node_color)
        enddo

        allocate(s_buffer(2*ninter))
        allocate(r_buffer(2*ninter*nspmd))
        s_buffer(1:2*ninter) = 0
        r_buffer(1:2*ninter*nspmd) = 0
        do j=1,type7_nb
          i = type7_list(j)
          s_buffer((i-1)*2+1) = component(i)%s_comp_nb
          s_buffer((i-1)*2+2) = component(i)%m_comp_nb       
          component(i)%total_s_remote_comp_nb = component(i)%s_comp_nb
          component(i)%total_m_remote_comp_nb = component(i)%m_comp_nb
        enddo

        my_size = ninter*2
        do i=1,nspmd
          if(i-1/=ispmd) then
            call spmd_send(s_buffer, my_size, i-1, my_tag, SPMD_COMM_WORLD)
            call spmd_recv(r_buffer((i-1)*2*ninter+1), my_size, i-1, my_tag, SPMD_COMM_WORLD)   
          endif
        enddo

        do k=1,nspmd
          if(k-1/=ispmd) then
            do j=1,type7_nb
              i = type7_list(j)
              my_size = r_buffer((k-1)*2*ninter+(i-1)*2+1)
              component(i)%proc_comp(k)%remote_s_comp_nb = my_size            
              component(i)%total_s_remote_comp_nb = component(i)%total_s_remote_comp_nb + my_size
              allocate(component(i)%proc_comp(k)%remote_s_comp(6,my_size))
              my_size = r_buffer((k-1)*2*ninter+(i-1)*2+2)
              component(i)%proc_comp(k)%remote_m_comp_nb = my_size
              component(i)%total_m_remote_comp_nb = component(i)%total_m_remote_comp_nb + my_size
              allocate(component(i)%proc_comp(k)%remote_m_comp(6,my_size))

              my_size = component(i)%proc_comp(k)%remote_s_comp_nb + component(i)%proc_comp(k)%remote_m_comp_nb
              if(my_size>0) component(i)%proc_comp(k)%need_comm0 = .true.         
              allocate(component(i)%proc_comp(k)%remote_comp(6*my_size))
            enddo
          endif
        enddo
        return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine inter_init_component
      end module inter_init_component_mod
