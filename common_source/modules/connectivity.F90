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
!||    connectivity_mod              ../common_source/modules/connectivity.F90
!||--- called by ------------------------------------------------------
!||    asspar4                       ../engine/source/assembly/asspar4.F
!||    detach_node                   ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_from_interfaces   ../engine/source/engine/node_spliting/detach_node.F90
!||    detach_node_from_shells       ../engine/source/engine/node_spliting/detach_node.F90
!||    find_segment_in_list          ../engine/source/engine/node_spliting/detach_node.F90
!||    init_ghost_shells             ../engine/source/engine/node_spliting/ghost_shells.F90
!||    radioss2                      ../engine/source/engine/radioss2.F
!||    rdresb                        ../engine/source/output/restart/rdresb.F
!||    resol                         ../engine/source/engine/resol.F
!||    resol_head                    ../engine/source/engine/resol_head.F
!||    restalloc                     ../engine/source/output/restart/arralloc.F
!||    set_new_node_values           ../engine/source/engine/node_spliting/detach_node.F90
!||    spmd_exchange_ghost_shells    ../engine/source/engine/node_spliting/ghost_shells.F90
!||    test_jc_shell_detach          ../engine/source/engine/node_spliting/detach_node.F90
!||    update_pon_shells             ../engine/source/engine/node_spliting/update_pon.F90
!||    wrrestp                       ../engine/source/output/restart/wrrestp.F
!||--- uses       -----------------------------------------------------
!||    parith_on_mod                 ../common_source/modules/parith_on_mod.F90
!||    precision_mod                 ../common_source/modules/precision_mod.F90
!||====================================================================
      module connectivity_mod
        use iso_c_binding
        USE parith_on_mod
        use precision_mod, only: wp
!       INTEGER, PARAMETER :: NIXS = 11
!       INTEGER, PARAMETER :: NIXC = 7
!       INTEGER, PARAMETER :: NIXQ = 7
!       INTEGER, PARAMETER :: NIXT = 5
!       INTEGER, PARAMETER :: NIXP = 6
!       INTEGER, PARAMETER :: NIXR = 6
!       INTEGER, PARAMETER :: NIXTG = 6
!       INTEGER, PARAMETER :: NIXUR = 6
        type shell_
          ! old storage of shells
          integer, dimension(:,:), allocatable :: ixc !< ixc(1,i) : Material ID of the i-th shell element
                                                      !< ixc(2:5,i) :  nodes of the i-th shell element
                                                      !< ixc(6,i) :  PID of the i-th shell element 
                                                      !< ixc(7,i) :  user id of the shell element
         ! new storage of shells
          integer, dimension(:,:), allocatable :: nodes !< nodes(1:4,i) :  nodes of the i-th shell element
          integer, dimension(:), allocatable :: pid !< pid(i) :  PID of the i-th shell element
          integer, dimension(:), allocatable :: matid !< matid(i) :  Material ID of the i-th shell element
          integer, dimension(:), allocatable :: user_id !< user_id(i) :  user id of the shell element
          real(kind=wp), dimension(:), allocatable :: damage
          real, dimension(:), allocatable :: dist_to_center !< maximum distance of a node to the center of the element 
          integer, dimension(:), allocatable :: permutation !< permutation of the shell element in order to have the shells sorted by user_id
          integer :: offset
          type(C_PTR) :: loc2glob
        end type shell_

        type list_of_shells_
          integer, dimension(:), allocatable :: index !< local id of the shell element to send to the other process
        end type list_of_shells_
        type ghost_shell_
          integer, dimension(:,:), allocatable :: nodes !< nodes(1:4,i) :  nodes of the i-th shell element
          real(kind=wp), dimension(:), allocatable :: damage 
          type(list_of_shells_), dimension(:), allocatable :: shells_to_send !< local id of the shell element to send to the other process
          integer, dimension(:), allocatable :: offset !< offset of the shell element to receive from the other process 
          integer, dimension(:), allocatable :: addcnel !< address for the node to elemenent (shell) connectivity
          integer, dimension(:), allocatable :: cnel ! element index in nodes arrays
        end type ghost_shell_


        type solid_
          ! old storage of solids
          integer, dimension(:,:), allocatable :: ixs !< ixs(1,i) : Material ID of the i-th solid element
                                                      !< ixs(2:9,i) :  nodes of the i-th solid element
                                                      !< ixs(10,i) :  PID of the i-th solid element 
                                                      !< ixs(11,i) :  user id of the solid element
          !new storage of solids
          integer, dimension(:,:), allocatable :: nodes !< nodes(1:8,i) :  nodes of the i-th solid element
          integer, dimension(:), allocatable :: pid !< pid(i) :  PID of the i-th solid element
          integer, dimension(:), allocatable :: matid !< matid(i) :  Material ID of the i-th solid element
          integer, dimension(:), allocatable :: user_id !< user_id(i) :  user id of the solid element
          type(C_PTR) :: loc2glob
        end type solid_

        type connectivity_
          type(shell_) :: shell
          type(ghost_shell_) :: ghost_shell
          type(solid_) :: solid
          type(element_pon_) :: pon
        end type connectivity_ 
        contains 

!! \brief extend nodal arrays                                                              
!||====================================================================
!||    init_global_shell_id   ../common_source/modules/connectivity.F90
!||--- called by ------------------------------------------------------
!||    rdresb                 ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    reserve_capacity       ../common_source/tools/container/umap_mod.F90
!||    stlsort_int_int        ../common_source/tools/sort/cppsort.cpp
!||--- uses       -----------------------------------------------------
!||    umap_mod               ../common_source/tools/container/umap_mod.F90
!||====================================================================
        subroutine init_global_shell_id(shell)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use umap_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            type(shell_) :: shell !< connectivity of elements
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
            integer :: i
            integer :: numelc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            shell%loc2glob = create_umap()
            numelc = size(shell%user_id)
            allocate(shell%permutation(numelc))
            call reserve_capacity(shell%loc2glob, numelc)
            do i = 1, numelc
              call add_entry_umap(shell%loc2glob, shell%user_id(i), i)
              shell%permutation(i) = i
            end do
            CALL STLSORT_INT_INT(numelc,shell%user_id,shell%permutation)
        end subroutine init_global_shell_id


!||====================================================================
!||    get_local_shell_id   ../common_source/modules/connectivity.F90
!||--- uses       -----------------------------------------------------
!||    umap_mod             ../common_source/tools/container/umap_mod.F90
!||====================================================================
        function get_local_shell_id(shell, global_id) result(local_id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use umap_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
            type(shell_) :: shell!< nodal arrays
            integer, intent(in) :: global_id !< global id
            integer :: local_id !< local id or 0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
            local_id = get_value_umap(shell%loc2glob, global_id, 0)
        end function get_local_shell_id


      end module
