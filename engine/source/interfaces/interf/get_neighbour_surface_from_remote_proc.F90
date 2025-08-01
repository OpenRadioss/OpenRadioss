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
!||    get_neighbour_surface_from_remote_proc_mod   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||--- called by ------------------------------------------------------
!||    spmd_exch_neighbour_segment                  ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||====================================================================
      module get_neighbour_surface_from_remote_proc_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine finds the remote connected segments connected by the 2 nodes to a new active segments
!! \details for each new active segment of an interface /TYPE25 and for the 4 edges of the segment :
!!          * find the list of remote connected segments without neighbour
!!          * check if the remote connected segments has no neighbour
!!          * if yes, the remote connected segments is sent to the other procs
!||====================================================================
!||    get_neighbour_surface_from_remote_proc   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||--- called by ------------------------------------------------------
!||    spmd_exch_neighbour_segment              ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||--- calls      -----------------------------------------------------
!||    alloc_my_real_1d_array                   ../common_source/modules/array_mod.F
!||    dealloc_my_real_1d_array                 ../common_source/modules/array_mod.F
!||    get_convexity_normals                    ../engine/source/interfaces/interf/get_convexity_normals.F90
!||    get_segment_interface_id                 ../engine/source/interfaces/interf/get_segment_interface_id.F90
!||    get_segment_normal                       ../engine/source/interfaces/interf/get_segment_normal.F90
!||--- uses       -----------------------------------------------------
!||    array_mod                                ../common_source/modules/array_mod.F
!||    constant_mod                             ../common_source/modules/constant_mod.F
!||    get_convexity_normals_mod                ../engine/source/interfaces/interf/get_convexity_normals.F90
!||    get_segment_edge_mod                     ../engine/source/interfaces/interf/get_segment_edge.F90
!||    get_segment_interface_id_mod             ../engine/source/interfaces/interf/get_segment_interface_id.F90
!||    get_segment_normal_mod                   ../engine/source/interfaces/interf/get_segment_normal.F90
!||    intbufdef_mod                            ../common_source/modules/interfaces/intbufdef_mod.F90
!||    nodal_arrays_mod                         ../common_source/modules/nodal_arrays.F90
!||    precision_mod                            ../common_source/modules/precision_mod.F90
!||    shooting_node_mod                        ../engine/share/modules/shooting_node_mod.F
!||====================================================================
        subroutine get_neighbour_surface_from_remote_proc( ninter,numnod,nspmd,  &
                                                             size_r_buffer,nb_r_segment,s_buffer_2_size, &
                                                             nodes,r_buffer,s_buffer_2, &
                                                             x,intbuf_tab,shoot_struct ,&
                                                             ispmd,proc_id_0 )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero,ep30
          use intbufdef_mod , only : intbuf_struct_
          use shooting_node_mod , only : shooting_node_type
          use get_segment_interface_id_mod , only : get_segment_interface_id
          use get_segment_normal_mod , only : get_segment_normal
          use get_convexity_normals_mod , only : get_convexity_normals
          use get_segment_edge_mod , only : get_segment_edge
          use array_mod , only : array_type,alloc_my_real_1d_array,dealloc_my_real_1d_array
          use nodal_arrays_mod, only : get_local_node_id, nodal_arrays_
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ninter !< number of interface
          integer, intent(in) :: numnod !< number of node
          integer, intent(in) :: nspmd !< total number of mpi tasks
          integer, intent(in) :: size_r_buffer !< size of r_buffer
          integer, intent(in) :: nb_r_segment !< number of new remote segment
          integer, intent(in) :: proc_id_0 !< S processor id
          integer, intent(in) :: ispmd !< current processor id
          integer, dimension(3,nspmd), intent(inout) :: s_buffer_2_size !< size of s_buffer_2
!         integer, dimension(numnod), intent(in) :: itabm1 !< global to local node id
          type(nodal_arrays_), intent(in) :: nodes !< nodal arrays
          real(kind=WP), dimension(size_r_buffer) :: r_buffer !< mpi buffer (rcv)
          real(kind=WP), dimension(3,numnod), intent(in) :: x !< nodal position
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab    !< interface data 
          type(shooting_node_type), intent(inout) :: shoot_struct !< structure for shooting node algo
          type(array_type), dimension(nspmd), intent(inout) :: s_buffer_2 !< mpi buffer (send)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,ijk,kji
          integer :: proc_id,proc_number
          integer :: n_segment_id,nb_connected_segment,next_segment
          integer :: nin
          integer :: my_address
          integer :: local_node_id_1,local_node_id_2
          integer :: global_node_id_1,global_node_id_2
          integer :: nb_surface_1,nb_surface_2,nb_result_intersect_0
          integer :: shift
          integer :: my_reduced_nb
          integer :: my_size,old_size
          integer, dimension(2,4), parameter :: egde_list = reshape( (/1,2,2,3,3,4,4,1/) , shape(egde_list) )
          integer, dimension(4) :: segment_node_id
          integer, dimension(nb_r_segment) :: address
          integer, dimension(nb_r_segment,5) :: list_r_segment
          integer, dimension(:), allocatable :: result_intersect_0
          integer, dimension(:), allocatable ::  intersect_1,intersect_2
          integer, dimension(:), allocatable :: n_iedge 
          integer, dimension(:,:), allocatable :: my_reduced_list,my_reduced_neighbour
          real(kind=WP) :: my_real_variable      
          real(kind=WP), dimension(3) :: segment_position ! coordinates of the segment barycentre
          real(kind=WP), dimension(:), allocatable :: my_real_tmp_array
          real(kind=WP), dimension(:,:), allocatable :: n_normal, n_vconvexity
#ifdef MYREAL8
          integer(kind=8) :: my_integer
          integer(kind=8) :: my_int_variable   
#else
          integer(kind=4) :: my_integer
          integer(kind=4) :: my_int_variable   
#endif
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
          integer , external :: dichotomic_search_i_asc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! mpi buffer s_buffer_2 / r_buffer_2 :
          ! for each new segment and for the 4 edges of the segment :
          ! [1] : global segment id
          ! [2] : local segment id
          ! [3] : node id 1
          ! [4] : node id 2
          ! [5] : interface id nin
          ! [6] : edge id
          ! [7] : number of remote connected segment for the edge [6]
          ! [7+1:7+[7]] :  global id of the remote connected segment
          ! [7+[7]+1:7+2*[7]] :  local id of the remote connected segments
          ! [7+2*[7]+1:7+5*[7]] :  normals of the remote connected segments
          ! [7+5*[7]+1:7+6*[7]] :  edge id of the remote connected segments
          ! [7+6*[7]+1:7+10*[7]] : boolean for the remote segment neighbourhood, 4 values per segment, 1 per edge --> 0=no neighbour, 1=already a neighbour

          allocate( result_intersect_0( shoot_struct%max_surf_nb ) )
          allocate( intersect_1( shoot_struct%max_surf_nb ) )
          allocate( intersect_2( shoot_struct%max_surf_nb ) )

          next_segment = 0
          do i=1,nb_r_segment
            address(i) = next_segment

            my_integer = transfer(r_buffer(next_segment + 7),my_int_variable) ! get the number of r connected segment
            nb_connected_segment = my_integer
            list_r_segment(i,1) = nb_connected_segment

            my_integer = transfer(r_buffer(next_segment+3),my_int_variable) ! get the global node id "node 1"
            list_r_segment(i,2) = my_integer
            my_integer = transfer(r_buffer(next_segment+4),my_int_variable) ! get the global node id "node 2"
            list_r_segment(i,3) = my_integer
            my_integer = transfer(r_buffer(next_segment+5),my_int_variable) ! get the interface id
            list_r_segment(i,4) = my_integer
            my_integer = transfer(r_buffer(next_segment+8),my_int_variable) ! get the number of procs with the 2 nodes "node_id_1" & "node_id_2"
            proc_number = my_integer
            list_r_segment(i,5) = proc_number 
            next_segment = next_segment + 8 + 13*nb_connected_segment+3 + proc_number + 3
          enddo
          ! -------------------------
          ! loop over the remote segment/surface
          do i=1,nb_r_segment
            global_node_id_1 = list_r_segment(i,2)
            global_node_id_2 = list_r_segment(i,3)
            nin = list_r_segment(i,4)
!            local_node_id_1 = sysfus2(global_node_id_1,itabm1,numnod) ! convert the global node id to local
!            local_node_id_2 = sysfus2(global_node_id_2,itabm1,numnod) ! convert the global node id to local
            local_node_id_1 = get_local_node_id(nodes,global_node_id_1)
            local_node_id_2 = get_local_node_id(nodes,global_node_id_2)

            ! ------
            ! 1srt node
            nb_surface_1 = shoot_struct%shift_m_node_surf(local_node_id_1+1) - shoot_struct%shift_m_node_surf(local_node_id_1)   ! get the number of surface for the node "node_id_1"
            shift = shoot_struct%shift_m_node_surf(local_node_id_1)
            intersect_1(1:nb_surface_1) = shoot_struct%m_node_surf( shift+1:shift+nb_surface_1 )
            ! ------

            ! ------
            ! 2nd node
            nb_surface_2 = shoot_struct%shift_m_node_surf(local_node_id_2+1) - shoot_struct%shift_m_node_surf(local_node_id_2)   ! get the number of surface for the node "node_id_2"
            shift = shoot_struct%shift_m_node_surf(local_node_id_2)
            intersect_2(1:nb_surface_2) = shoot_struct%m_node_surf( shift+1:shift+nb_surface_2 )
            ! ------

            ! ------
            if(nb_surface_1>0.and.nb_surface_2>0) then
              call intersect_2_sorted_sets( intersect_1,nb_surface_1, &
                                            intersect_2,nb_surface_2, &
                                            result_intersect_0,nb_result_intersect_0 )
            else
              nb_result_intersect_0 = 0
            endif
            ! ------

            allocate( n_normal(3,nb_result_intersect_0) )
            allocate( n_vconvexity(3,nb_result_intersect_0) )
            allocate( n_iedge(nb_result_intersect_0) )
            allocate( my_reduced_list(nb_result_intersect_0,2) )
            allocate( my_reduced_neighbour(nb_result_intersect_0,4) )

            call get_segment_interface_id( ninter,nb_result_intersect_0,result_intersect_0, &
                                           nin,my_reduced_nb,my_reduced_list,my_reduced_neighbour, &
                                           shoot_struct,intbuf_tab,local_node_id_1,local_node_id_2,n_iedge)

            do ijk=1,my_reduced_nb
              ! segment/surface orientation
              n_segment_id = my_reduced_list(ijk,1) ! connected segment id
              ! compute the normal to the segment "n_segment_id"
              call get_segment_normal( n_segment_id,segment_node_id,segment_position,n_normal(1,ijk),intbuf_tab(nin),numnod,x )
              ! compute the tangent vector to the segment around the edge "n_segment_id" 
              call get_convexity_normals( local_node_id_1,local_node_id_2,n_normal(1,ijk),n_vconvexity(1,ijk),numnod,x )
            enddo

            proc_number = list_r_segment(i,5)
            do j=1,proc_number             
              my_address = address(i) + 8+6*list_r_segment(i,1) + 3
              my_integer = transfer(r_buffer(my_address+j),my_int_variable) ! get the processor id
              proc_id = my_integer
              my_size = 0
              if(proc_id_0/=ispmd+1) then
                ! -----------
                ! check if the size is enough
                my_size = 7+13*my_reduced_nb ! get the mpi buffer size for the current new segment

                if(s_buffer_2_size(1,proc_id)+my_size>s_buffer_2(proc_id)%size_my_real_array_1d) then
                  old_size = s_buffer_2(proc_id)%size_my_real_array_1d
                  allocate( my_real_tmp_array(old_size) )
                  my_real_tmp_array(1:old_size) = s_buffer_2(proc_id)%my_real_array_1d(1:old_size)
                  call dealloc_my_real_1d_array(s_buffer_2(proc_id))
                  s_buffer_2(proc_id)%size_my_real_array_1d = 2*(s_buffer_2(proc_id)%size_my_real_array_1d + my_size) + 1
                  call alloc_my_real_1d_array(s_buffer_2(proc_id))
                  s_buffer_2(proc_id)%my_real_array_1d(1:old_size) = my_real_tmp_array(1:old_size)
                  deallocate( my_real_tmp_array )
                endif
                ! -----------

                my_address = s_buffer_2_size(1,proc_id)
                s_buffer_2(proc_id)%my_real_array_1d(my_address+1:my_address+6) = r_buffer(1+address(i):6+address(i)) ! save the data of the new segment

                my_integer = my_reduced_nb
                s_buffer_2(proc_id)%my_real_array_1d(my_address+7) = transfer(my_integer,my_real_variable) ! save the number of remote connected segment

                do ijk=1,my_reduced_nb
                  n_segment_id = my_reduced_list(ijk,1) ! connected segment id
                  my_integer = -intbuf_tab(nin)%mseglo(n_segment_id)
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+7+ijk) = transfer(my_integer,my_real_variable) ! save the global remote segment id
                enddo
                my_address = my_address + 7 + my_reduced_nb
                do ijk=1,my_reduced_nb
                  n_segment_id = my_reduced_list(ijk,1) ! connected segment id
                  my_integer = n_segment_id
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+ijk) = transfer(my_integer,my_real_variable) ! save the local remote segment id   
                enddo
                my_address = my_address + my_reduced_nb
                do ijk=1,my_reduced_nb ! save the 3 normals of remote segment
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+1) = n_normal(1,ijk)
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+2) = n_normal(2,ijk)
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+3) = n_normal(3,ijk)
                  my_address = my_address + 3
                enddo

                do ijk=1,my_reduced_nb ! save the edge of the remote connected segment
                  my_integer = n_iedge(ijk)
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+ijk) = transfer(my_integer,my_real_variable) ! edge of the connected segment
                enddo

                my_address = my_address + my_reduced_nb
                do ijk=1,my_reduced_nb 
                  do kji=1,4
                    my_integer =my_reduced_neighbour(ijk,kji)
                    s_buffer_2(proc_id)%my_real_array_1d(my_address+kji) = transfer(my_integer,my_real_variable) ! boolean for neirbourhood
                  enddo
                  my_address = my_address + 4
                enddo

                do ijk=1,my_reduced_nb
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+1) = n_vconvexity(1,ijk) ! neighbour segment normal (x)
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+2) = n_vconvexity(2,ijk) ! neighbour segment normal (y)
                  s_buffer_2(proc_id)%my_real_array_1d(my_address+3) = n_vconvexity(3,ijk) ! neighbour segment normal (z)    
                  my_address = my_address + 3        
                enddo

                s_buffer_2_size(1,proc_id) = s_buffer_2_size(1,proc_id)+my_size ! size of mpi buffer
                s_buffer_2_size(2,proc_id) = s_buffer_2_size(2,proc_id)+my_reduced_nb ! total number of connected remote sgment
                s_buffer_2_size(3,proc_id) = s_buffer_2_size(3,proc_id)+1 ! total number of connected remote sgment
              endif
            enddo
            deallocate( n_normal )
            deallocate( n_iedge )
            deallocate( my_reduced_list )
            deallocate( my_reduced_neighbour )
              deallocate( n_vconvexity )
          enddo

          ! --------------------------

          deallocate( result_intersect_0 )
          deallocate( intersect_1 )
          deallocate( intersect_2 )
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_neighbour_surface_from_remote_proc
      end module get_neighbour_surface_from_remote_proc_mod
