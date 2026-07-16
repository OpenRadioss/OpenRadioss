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
!||====================================================================
!||    init_global_boundary_list_mod   ../engine/source/mpi/init/init_global_boundary_list.F90
!||--- called by ------------------------------------------------------
!||    resol                           ../engine/source/engine/resol.F
!||====================================================================
      module init_global_boundary_list_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initialize the list of global boundary nodes
!! \details
!||====================================================================
!||    init_global_boundary_list   ../engine/source/mpi/init/init_global_boundary_list.F90
!||--- called by ------------------------------------------------------
!||    resol                       ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod                ../common_source/tools/memory/my_alloc.F90
!||    my_dealloc_mod              ../common_source/tools/memory/my_dealloc.F90
!||    nodal_arrays_mod            ../common_source/modules/nodal_arrays.F90
!||====================================================================
        subroutine init_global_boundary_list(numnod,nspmd,nodes)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use nodal_arrays_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod
          use my_dealloc_mod, only : my_dealloc
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: nspmd !< number of mpi tasks
          type(nodal_arrays_), intent(inout) :: nodes !< nodal arrays
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,node_id,next
          integer, dimension(:), allocatable :: tag_node,tmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          call my_alloc(tmp, numnod, "tmp")
          call my_alloc(tag_node, numnod, "tag_node")
          tag_node(1:numnod) = 0
          next = 0
          do i=nodes%boundary_add(1,1),nodes%boundary_add(1,nspmd+1)-1
            node_id = nodes%boundary(i)
            if(tag_node(node_id)==0) then
              tag_node(node_id) = 1
              next = next + 1
              tmp(next) = node_id
            endif
          enddo
          call my_alloc(nodes%global_boundary, next, "nodes%global_boundary")
          nodes%global_boundary(1:next) = tmp(1:next)
          nodes%global_boundary_nb = next
          call my_dealloc(tmp)
          call my_dealloc(tag_node)

        end subroutine init_global_boundary_list
      end module init_global_boundary_list_mod
