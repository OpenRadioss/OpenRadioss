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
      module init_ale_spmd_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief
!! \details
        subroutine init_ale_spmd(nv46,n2d,numels,numelq,numnod, &
                                 nspmd,nsvois,nqvois,s_lesdvois,s_lercvois,nesdvois,nercvois, &
                                 lesdvois,lercvois,itab,itabm1,ixs,ixq,ale_connect )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ale_connectivity_mod , only : t_ale_connectivity
          use element_mod , only : nixs, nixq
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nv46 !< Number of faces per element (4 for 2D, 6 for 3D)
          integer, intent(in) :: n2d !< Flag for 2D/3D ALE (0 for 3D, 1 for 2D)
          integer, intent(in) :: numels !< Number of solid elements
          integer, intent(in) :: numelq !< Number of fluid elements
          integer, intent(in) :: numnod !< Number of nodes
          integer, intent(in) :: nspmd !< Number of processors
          integer, intent(in) :: nsvois !< number of frontier solid elements
          integer, intent(in) :: nqvois !< number of frontier quad elements
          integer, intent(in) :: s_lesdvois !< size of lesdvois array
          integer, intent(in) :: s_lercvois !< size of lercvois array             
          integer, dimension(nspmd+1), intent(in) :: nesdvois !< number of frontier elements (send)          
          integer, dimension(nspmd+1), intent(in) :: nercvois !< number of frontier elements (rcv)
          integer, dimension(s_lesdvois), intent(in) :: lesdvois !< frontier element ids (send)
          integer, dimension(s_lercvois), intent(in) :: lercvois !< frontier element ids (rcv)
          integer, dimension(numnod), intent(in) :: itab !< local node ID to global node ID mapping
          integer, dimension(2*numnod), intent(in) :: itabm1 !< local node ID to user node ID mapping
          integer, dimension(nixs,numels), intent(in) :: ixs !< Solid element connectivity
          integer, dimension(nixq,numelq), intent(in) :: ixq !< Quad element connectivity          
          type(t_ale_connectivity), intent(inout) :: ale_connect !< ALE data structure for connectivity
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: n_entity,remote_elm,nb_node,nb_node_facet
          integer :: i,k,ijk,kk,l,lencom
          integer :: my_address,remote_elem_id,find_it,remote_neighb_elem
          integer :: nb_connected_elm
          integer, dimension(:,:), allocatable :: neighbor_node
          integer, dimension(:), allocatable :: tag
          integer, dimension(:), allocatable :: ni

          integer, parameter :: permutation_3d(4,6) = reshape( [3,1,2,4,  &
                                                                7,4,3,8,  &
                                                                6,8,7,5,  &
                                                                2,5,6,1,  &
                                                                7,2,6,3,  &
                                                                8,1,4,5], &
                                                                [4,6] )  
          integer, parameter :: permutation_2d(2,4) = reshape( [1,2,  &
                                                                2,3,  &
                                                                3,4,  &
                                                                4,1], &
                                                                [2,4] )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
          integer, external :: sysfus2                                                                
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          lencom = nercvois(nspmd+1)+nesdvois(nspmd+1)
          if(n2d==0) then
            n_entity = numels
            remote_elm = nsvois
            nb_node = 8
            nb_node_facet = 4
          else
            n_entity = numelq
            remote_elm = nqvois
            nb_node = 4
            nb_node_facet = 2
          end if
          allocate(neighbor_node(n_entity+remote_elm,nb_node))

          neighbor_node(1:n_entity+remote_elm,1:nb_node) = 0

          remote_neighb_elem = 0
          do i=1,n_entity
            my_address = ale_connect%ee_connect%iad_connect(i)
            nb_connected_elm = ale_connect%ee_connect%iad_connect(i+1) - ale_connect%ee_connect%iad_connect(i) ! get the number of connected ALE element
            if(nb_connected_elm>0) then
              do k=1,nb_connected_elm
                if(ale_connect%ee_connect%connected(my_address+k-1) > n_entity) then
                  remote_neighb_elem = remote_neighb_elem + 1
                end if
              enddo
            endif
          end do

          allocate(ale_connect%ee_connect%index_elem_w_neigh(remote_neighb_elem))
          allocate(ale_connect%ee_connect%id_facet_neighbor_elem(nv46*remote_neighb_elem))
          ale_connect%ee_connect%id_facet_neighbor_elem(1:nv46*remote_neighb_elem) = 0
          ale_connect%ee_connect%remote_neighb_elem = remote_neighb_elem
          remote_neighb_elem = 0
          do i=1,n_entity
            my_address = ale_connect%ee_connect%iad_connect(i)
            nb_connected_elm = ale_connect%ee_connect%iad_connect(i+1) - ale_connect%ee_connect%iad_connect(i) ! get the number of connected ALE element
            if(nb_connected_elm>0) then       
              do k=1,nb_connected_elm
                if(ale_connect%ee_connect%connected(my_address+k-1) > n_entity) then
                  remote_neighb_elem = remote_neighb_elem + 1
                  ale_connect%ee_connect%index_elem_w_neigh(remote_neighb_elem) = i
                  if(n2d==0) then
                    neighbor_node(i,1:nb_node) = itab(ixs(2:nb_node+1,i))
                  else
                    neighbor_node(i,1:nb_node) = itab(ixq(2:nb_node+1,i))
                  endif           
                end if
              enddo
            endif
          end do
          if(n2d==0) then
            call spmd_i8vois(neighbor_node,nercvois,nesdvois,lercvois,lesdvois,lencom)
          else
            call spmd_i4vois(neighbor_node,nercvois,nesdvois,lercvois,lesdvois,lencom)
          endif

          allocate(tag(0:numnod))
          tag(0:numnod) = 0

          allocate(ni(nv46))
          ni(1:nv46) = 0

          do ijk=1,remote_neighb_elem
            i = ale_connect%ee_connect%index_elem_w_neigh(ijk)
            my_address = ale_connect%ee_connect%iad_connect(i)
            nb_connected_elm = ale_connect%ee_connect%iad_connect(i+1) - ale_connect%ee_connect%iad_connect(i) ! get the number of connected ALE element
            if(nb_connected_elm>0) then         
              do k=1,nb_connected_elm
                remote_elem_id = ale_connect%ee_connect%connected(my_address+k-1)                              
                if(remote_elem_id > n_entity) then
                  if(neighbor_node(remote_elem_id,1)/=0) then
                    do kk=1,nv46          
                      ni(1:nv46) = 0                          
                      do l=1,nb_node_facet
                        if(n2d==0) then
                          ni(l) = sysfus2(neighbor_node(remote_elem_id,permutation_3d(l,kk)),itabm1,numnod)
                        else
                          ni(l) = sysfus2(neighbor_node(remote_elem_id,permutation_2d(l,kk)),itabm1,numnod)
                        end if
                        tag(ni(l)) = 1
                      enddo  

                      find_it = 0             
                      do l=1,nb_node_facet                                     
                        if(n2d==0) then
                          find_it = find_it + tag(ixs(1+permutation_3d(l,k),i))
                        else                 
                          find_it = find_it + tag(ixq(1+permutation_2d(l,k),i))
                        end if
                      enddo
                      do l=1,nb_node_facet
                        tag(ni(l)) = 0
                      enddo 

                      if(find_it==nb_node_facet) then
                        ale_connect%ee_connect%id_facet_neighbor_elem(nv46*(ijk-1)+k) = kk
                        exit
                      endif
                    enddo
                  end if
                endif
              enddo
            endif
          end do 


          deallocate(tag)
          deallocate(ni)
          deallocate(neighbor_node)

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_ale_spmd
      end module init_ale_spmd_mod
