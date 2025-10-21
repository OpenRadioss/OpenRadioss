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
!||    shooting_node_mod                        ../engine/share/modules/shooting_node_mod.F
!||--- called by ------------------------------------------------------
!||    check_edge_state                         ../engine/source/interfaces/interf/check_edge_state.F
!||    check_remote_surface_state               ../engine/source/interfaces/interf/check_remote_surface_state.F
!||    check_surface_state                      ../engine/source/interfaces/interf/check_surface_state.F
!||    count_nb_elem_edge                       ../engine/source/interfaces/interf/count_nb_elem_edge.F
!||    dealloc_shoot_inter                      ../engine/source/interfaces/interf/dealloc_shoot_inter.F
!||    find_edge_from_remote_proc               ../engine/source/interfaces/interf/find_edge_from_remote_proc.F
!||    find_edge_inter                          ../engine/source/interfaces/interf/find_edge_inter.F
!||    find_surface_from_remote_proc            ../engine/source/interfaces/interf/find_surface_from_remote_proc.F
!||    find_surface_inter                       ../engine/source/interfaces/interf/find_surface_inter.F
!||    get_neighbour_surface                    ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||    get_segment_interface_id                 ../engine/source/interfaces/interf/get_segment_interface_id.F90
!||    get_segment_orientation                  ../engine/source/interfaces/interf/get_segment_orientation.F90
!||    init_nodal_state                         ../engine/source/interfaces/interf/init_nodal_state.F
!||    resol                                    ../engine/source/engine/resol.F
!||    spmd_exch_deleted_surf_edge              ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
!||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||    tagoff3n                                 ../engine/source/interfaces/interf/chkstfn3.F
!||    update_neighbour_segment                 ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||====================================================================
      module shooting_node_mod
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Definition of the shooting node data structure
        implicit none
        type offset_elem_
          integer :: sol_up_bound !< upper bound for solid
          integer :: sol_low_bound !< lower bound for solid
          integer :: quad_up_bound !< upper bound for quad
          integer :: quad_low_bound!< lower bound for quad
          integer :: shell_up_bound !< upper bound for shell
          integer :: shell_low_bound!< lower bound for shell
          integer :: truss_up_bound !< upper bound for truss
          integer :: truss_low_bound!< lower bound for truss
          integer :: beam_up_bound !< upper bound for beam
          integer :: beam_low_bound!< lower bound for beam
          integer :: spring_up_bound !< upper bound for spring
          integer :: spring_low_bound!< lower bound for spring
          integer :: shell3n_up_bound !< upper bound for shell3n
          integer :: shell3n_low_bound!< lower bound for shell3n
        end type offset_elem_

        type working_array
          integer, dimension(:), allocatable :: surf  ! working array for surface
          integer, dimension(:), allocatable :: proc  ! working array for processor            
        end type working_array

        type remote_elm
          integer, dimension(:), allocatable :: remote_elm_m  ! list of remote element for the main edges
          integer, dimension(:), allocatable :: remote_elm_s  ! list of remote element for the secondary edges
          integer, dimension(:), allocatable :: nb_elm_m      ! number of element connected to the segment/surface (only for interface 25)
        end type remote_elm

        type neighbour_
          integer :: hash_id !< id of the hashtable (used only for the /TYPE25)
          integer, dimension(:), allocatable :: seg_nb !< number of neighbouring segments, size = 4*nrtm
          integer, dimension(:), allocatable :: seg_index !< pointer to mvoisin_index array, size = 4*nrtm+4
          integer, dimension(:), allocatable :: mvoisin_index !< pointer to mvoisin array, size = 4*nrtm
        end type neighbour_        

        type shooting_node_type 
          ! ------------------------------
          ! secondary node array
          integer :: size_sec_node ! size of inter_sEC_node & SEC_node_ID
          integer, dimension(:), allocatable :: shift_s_node ! shift to point to inter_sEC_node/SEC_node_ID arrays & number of interface per node
          integer, dimension(:), allocatable :: inter_sec_node ! list of interface of the nodes
          integer, dimension(:), allocatable :: SEC_node_ID ! ID of secondary nodes in each interface
          ! ------------------------------
          ! main node array
          integer :: max_proc_nb      ! maximum number of processor
          integer :: size_m_node_proc ! size of M_node_proc
          integer, dimension(:), allocatable :: shift_m_node_proc ! shift to point to M_node_proc array & number of processor per node
          integer, dimension(:), allocatable :: M_node_proc       ! list of processor per node

          integer :: max_surf_nb      ! maximun number of surface
          integer :: size_m_node_surf ! size of M_node_surf
          integer, dimension(:), allocatable :: shift_m_node_surf ! shift to point to M_node_surf array & number of surface per node
          integer, dimension(:), allocatable :: M_node_surf       ! list of surface per node

          integer :: max_edge_nb      ! maximun number of edge
          integer :: size_m_node_edge ! size of M_node_edge
          integer :: size_s_node_edge ! size of s_node_edge
          integer, dimension(:), allocatable :: shift_m_node_edge ! shift to point to M_node_edge array & number of edge per node
          integer, dimension(:), allocatable :: shift_s_node_edge ! shift to point to M_node_edge array & number of edge per node
          integer, dimension(:), allocatable :: m_node_edge       ! list of edge per node (M node)
          integer, dimension(:), allocatable :: s_node_edge       ! list of edge per node (S node)

          integer, dimension(:,:), allocatable :: shift_interface   ! shift for surface ID
          integer, dimension(:), allocatable :: shift_interface2   ! shift for surface ID (global Ids)
          ! ------------------------------
          ! surface array
          integer :: size_remote_surf ! size of array remote_surf
          integer :: number_remote_surf ! number of potential rmote surface for type 24 & 25
          integer, dimension(:), allocatable :: remote_surf ! list of potential rmote surface for type 24 & 25
          integer :: size_new_surf ! size of array new_surf
          integer :: number_new_surf ! number of new active surface for type 25
          integer, dimension(:), allocatable :: new_surf ! list of new active surface for type 25
          ! ------------------------------
          ! ------------------------------

          ! ------------------------------
          ! working array
          integer, dimension(:), allocatable :: global_nb_elem_off    ! number of deactivated element for each omp thread
          integer :: s_global_elem_index  ! size of "global_elem_IND" array
          integer, dimension(:), allocatable :: global_elem_index     ! list of deactivated element

          integer :: s_save_surface   ! size of save_surfACE array
          integer, dimension(:), allocatable :: save_surfACE ! list of deactivated surface
          integer :: save_surface_nb ! number of deactivated surface

          integer :: s_save_proc   ! size of save_proc array
          integer, dimension(:), allocatable :: save_proc ! list of processor with a deactivated surface
          integer :: save_proc_nb ! number of processor + the 4 nodes associated to the surfaces

          integer :: s_save_m_edge,s_save_s_edge   ! size of save_m_edge & save_s_edge array
          integer, dimension(:), allocatable :: save_m_edge ! list of deactivated edge : main node
          integer, dimension(:), allocatable :: save_s_edge ! list of deactivated edge : secondary node
          integer :: save_m_edge_nb,save_s_edge_nb ! number of deactivated edge

          integer :: s_save_proc_edge   ! size of save_proc_edge array
          integer, dimension(:), allocatable :: save_proc_edge ! list of processor with a deactivated edge
          integer :: save_proc_nb_edge ! number of processor + the 4 nodes associated to the surfaces


          type(remote_elm), dimension(:), allocatable :: inter
          type(offset_elem_) :: offset_elem !< upper and lower bound
          type(neighbour_), dimension(:), allocatable :: neighbour
        end type shooting_node_type 
! ----------------------------------------------------------------------------------------------------------------------
      end module shooting_node_mod
