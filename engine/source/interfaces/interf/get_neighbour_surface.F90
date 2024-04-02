!copyright>        openradioss
!copyright>        copyright (c) 1986-2024 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      module get_neighbour_surface_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine initializes the neighbourhood of a new active segment belonging to an interface /TYPE25
!! \details for each new active segment of an interface /TYPE25 and for the 4 edges of the segment :
!!          * find the list of local connected segments without neighbour
!!          * find the list of remote processor
!!          * exchange of data (new segment id + local connected segments)
!!          * remote processor : check if some segments without neighbour are connected to the new active segment
!!          * exchange of data (new segment id + remote connected segments)
!!          * compute for each pair (new segment + other segment) a criteria (angle between the 2 segment's normals)
!!          * choose the neighbour segmennt with the lower criteria
!!          * update the frontier for spmd exchange
        subroutine get_neighbour_surface( ispmd,nspmd,ninter25,npari,ninter,  &
                                          nbintc,nixs,numnod,numels,s_elem_state,nbddedgt,nbddedg_max,  &
                                          elem_state,ipari,intlist,itab,itabm1,newfront,ixs,iad_elem,x,         &
                                          intbuf_tab,spmd_arrays,shoot_struct  )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod , only : zero,ep30
          use shooting_node_mod , only : shooting_node_type
          use intbufdef_mod , only : intbuf_struct_
          use array_mod , only : array_type,alloc_my_real_1d_array,dealloc_my_real_1d_array,alloc_1d_array, &
                                 dealloc_1d_array
          use get_segment_normal_mod , only : get_segment_normal
          use get_segment_orientation_mod , only : get_segment_orientation
          use get_segment_criteria_mod , only : get_segment_criteria
          use get_segment_interface_id_mod , only : get_segment_interface_id
          use get_segment_edge_mod , only : get_segment_edge
          use spmd_update_frontier_int25_mod , only : spmd_update_frontier_int25
          use spmd_exch_neighbour_segment_mod , only : spmd_exch_neighbour_segment
          use update_neighbour_segment_mod , only : update_neighbour_segment
          use spmd_arrays_mod , only : spmd_arrays_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: ispmd !< processor id
          integer, intent(in) :: nspmd !< number of mpi processors
          integer, intent(in) :: ninter25 !< total number of interface /TYPE25
          integer, intent(in) :: npari !< 1rst dim of "ipari" array
          integer, intent(in) :: ninter !< total number of interface
          integer, intent(in) :: nbintc !< reduced number of interface (without /TYPE02)
          integer, intent(in) :: nixs !< 1rst dim of "ixs" array
          integer, intent(in) :: numnod !< total number of node
          integer, intent(in) :: numels !< number of solid element
          integer, intent(in) :: s_elem_state !< dim of elem_state
          integer, intent(inout) :: nbddedgt !< number of frontier edges
          integer, intent(inout) :: nbddedg_max !< number of frontier edges
          logical, dimension(s_elem_state), intent(in) :: elem_state !< state of the element : on or off
          integer, dimension(npari,ninter), intent(in) :: ipari !< interface data
          integer, dimension(nbintc), intent(in) :: intlist
          integer, dimension(numnod), intent(in) :: itab !< local to global node id array
          integer, dimension(numnod), intent(in) :: itabm1 !< global to local node id
          integer, dimension(ninter),intent(inout) :: newfront !< flag to force some exchanges related to S nodes between processor (if a S node becomes a shooting node - all interface) / force the collision detection algo if a new segment is activated for the (interface 25 + solid erosion)
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid element data
          integer, dimension(2,nspmd+1), intent(in) :: iad_elem !< frontier between processor
          my_real, dimension(3,numnod), intent(in) :: x !< nodal position
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab    !< interface data 
          type(spmd_arrays_), intent(inout) :: spmd_arrays !< structure for interface spmd arrays
          type(shooting_node_type), intent(inout) :: shoot_struct !< structure for shooting node algo
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,ijk,kji
          integer :: nin
          integer :: iedge,my_iedge
          integer :: segment_id,n_segment_id
          integer :: id_inter
          integer :: nb_proc_1,nb_proc_2
          integer :: node_id_1,node_id_2,node_id_3,node_id_4
          integer :: edge_number,shift
          integer :: my_address
          integer, dimension(nspmd) :: my_address_proc
          integer :: proc_id
          integer :: old_size
          integer :: nb_result_intersect_0,nb_result_intersect_2
          integer :: nb_surface_1,nb_surface_2
          integer :: my_reduced_nb
          integer :: my_offset,my_size
          integer :: number_inter
          integer, dimension(2,4), parameter :: egde_list = reshape( (/1,2,2,3,4,1,3,4/) , shape(egde_list) )
          integer, dimension(4) :: segment_node_id
          integer, dimension(:), allocatable :: n_iedge
          integer, dimension(:,:), allocatable :: my_reduced_list,my_reduced_neighbour
          integer, dimension(2,nspmd) :: s_buffer_size,r_buffer_size ! size of s/r buffer
          integer, dimension(3,nspmd) :: s_buffer_2_size,r_buffer_2_size ! size of s/r buffer
#ifdef MYREAL8
          integer(kind=8) :: my_integer
          integer(kind=8) :: my_int_variable   
#else
          integer(kind=4) :: my_integer
          integer(kind=4) :: my_int_variable   
#endif
          my_real :: my_real_variable
          my_real, dimension(:,:), allocatable :: n_normal
          my_real, dimension(3) :: segment_position
          integer, dimension(:), allocatable :: result_intersect_0,result_intersect_1,result_intersect_2
          integer, dimension(:), allocatable :: intersect_1,intersect_2
          integer, dimension(:), allocatable :: intersect_4,intersect_3
          integer, dimension(:,:), allocatable :: index_neighbour
          integer, dimension(:,:,:), allocatable :: iedge_min,segment_id_min
          my_real, dimension(:), allocatable :: my_real_tmp_array
          my_real, dimension(:,:), allocatable :: angle_min
          my_real, dimension(:,:), allocatable :: normal
          type(array_type), dimension(:), allocatable :: s_buffer,r_buffer ! mpi buffer 
          type(array_type), dimension(:), allocatable :: s_buffer_2,r_buffer_2 ! mpi buffer 

! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
          integer , external :: dichotomic_search_i_asc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          ! working array : surface
          allocate( result_intersect_0( shoot_struct%max_surf_nb ) )
          allocate( result_intersect_1( shoot_struct%max_surf_nb ) )
          allocate( intersect_1( shoot_struct%max_surf_nb ) )
          allocate( intersect_2( shoot_struct%max_surf_nb ) )
          allocate( index_neighbour( 4,shoot_struct%number_new_surf ) )
          index_neighbour( 1:4,1:shoot_struct%number_new_surf ) = 0
          ! working array : processor
          allocate( result_intersect_2( shoot_struct%max_proc_nb ) )
          allocate( intersect_3( shoot_struct%max_proc_nb ) )
          allocate( intersect_4( shoot_struct%max_proc_nb ) )
          ! working array : list of potential neighbour segment

          
          allocate( angle_min(4,shoot_struct%number_new_surf) )
          allocate( iedge_min(4,shoot_struct%number_new_surf,2) )
          allocate( segment_id_min(4,shoot_struct%number_new_surf,2) )
          allocate( normal(3,shoot_struct%number_new_surf) )
          angle_min(1:4,1:shoot_struct%number_new_surf) = ep30
          iedge_min(1:4,1:shoot_struct%number_new_surf,1:2) = 0
          segment_id_min(1:4,1:shoot_struct%number_new_surf,1:2) = 0
          normal(1:3,1:shoot_struct%number_new_surf) = zero

          ! mpi buffer 
          allocate( s_buffer(nspmd) )
          allocate( r_buffer(nspmd) )
          allocate( s_buffer_2(nspmd) )
          allocate( r_buffer_2(nspmd) )
          do i=1,nspmd
            s_buffer(i)%size_my_real_array_1d = 10*shoot_struct%max_surf_nb
            r_buffer(i)%size_my_real_array_1d = 0
            call alloc_my_real_1d_array(s_buffer(i))
          enddo
          do i=1,nspmd
            s_buffer_2(i)%size_my_real_array_1d = 0
            r_buffer_2(i)%size_my_real_array_1d = 0
          enddo
          ! mpi buffer s_buffer / r_buffer :
          ! for each new segment and for the 4 edges of the segment :
          ! [1] : global segment id
          ! [2] : local segment id
          ! [3] : node id 1
          ! [4] : node id 2
          ! [5] : interface id nin
          ! [6] : edge id
          ! [7] : number of local connected segment for the edge [6]
          ! [8] : number of remote processors
          ! [8+1:8+[7]] :  global id of the local connected segment
          ! [8+[7]+1:8+2*[7]] :  local id of the local connected segments
          ! [8+2*[7]+1:8+2*[7]+3] :  normals of the new active segment
          ! [8+2*[7]+3+1:8+5*[7]+3] :  normals of the local connected segments
          ! [8+5*[7]+3+1:8+6*[7]+3] :  edge id of the local connected segments
          ! [8+6*[7]+3+1:8+6*[7]+3+[8]] : list of remote processor
          ! [8+6*[7]+3+[8]+1:8+10*[7]+3+[8]] : boolean for the local segment neighbourhood, 4 values per segment, 1 per edge --> 0=no neighbour, 1=already a neighbour

          ! --------------------------

          ! --------------------------
          ! some initialization
          s_buffer_size(1:2,1:nspmd) = 0 ! size of S buffer
          r_buffer_size(1:2,1:nspmd) = 0 ! size of R buffer
          s_buffer_2_size(1:3,1:nspmd) = 0 ! size of S buffer
          r_buffer_2_size(1:3,1:nspmd) = 0 ! size of R buffer
          my_address_proc(1:nspmd) = 0 ! adress for the mpi buffer
          number_inter = shoot_struct%shift_interface(ninter+1,2)
          ! --------------------------

          ! --------------------------
          ! loop over the new active segment/surface to check the orienration of the segment
          do i=1,shoot_struct%number_new_surf
            k = shoot_struct%new_surf(i)  ! get the global surface id
            id_inter = dichotomic_search_i_asc(k, shoot_struct%shift_interface(1,1), number_inter+1) ! find the interface of the surface
            nin = shoot_struct%shift_interface(id_inter,2)
            segment_id = k - shoot_struct%shift_interface(id_inter,1) + 1 ! get the surface id in the nin interface

            newfront(nin) = -2

            do ijk=1,4
              intbuf_tab(nin)%evoisin(4*(segment_id-1)+ijk) = 0
              intbuf_tab(nin)%proc_mvoisin(4*(segment_id-1)+ijk) = 0
              intbuf_tab(nin)%mvoisin(4*(segment_id-1)+ijk) = 0
            enddo
            ! segment/surface orientation
            call get_segment_orientation( segment_id,s_elem_state,nixs,numels,numnod, &
                                            elem_state,ixs,x,intbuf_tab(nin) )
          enddo

          ! --------------------------
          ! loop over the new active segment/surface
          do i=1,shoot_struct%number_new_surf
            k = shoot_struct%new_surf(i)  ! get the global surface id
            id_inter = dichotomic_search_i_asc(k, shoot_struct%shift_interface(1,1), number_inter+1) ! find the interface of the surface
            nin = shoot_struct%shift_interface(id_inter,2)
            segment_id = k - shoot_struct%shift_interface(id_inter,1) + 1 ! get the surface id in the nin interface

            ! compute the normal to the segment "segment_id"
            call get_segment_normal( segment_id,segment_node_id,segment_position,normal(1,i),intbuf_tab(nin),numnod,x )
        
            ! check if the segment has 3 or 4 nodes (triangle or quadrangle)
            edge_number = 4
            node_id_3 = intbuf_tab(nin)%irectm(4*(segment_id-1)+3)
            node_id_4 = intbuf_tab(nin)%irectm(4*(segment_id-1)+4)
            if( node_id_3==node_id_4 ) edge_number = 3
            my_address = 0

            ! --------------------------
            ! loop over the edge of the segment
            do iedge = 1, edge_number
              ! ---------------
              ! intersection of list "list of segment for the node "node_id"

              ! ------
              ! 1srt node
              node_id_1 = intbuf_tab(nin)%irectm(4*(segment_id-1)+egde_list(1,iedge))
              nb_surface_1 = shoot_struct%shift_m_node_surf(node_id_1+1) - shoot_struct%shift_m_node_surf(node_id_1)   ! get the number of surface for the node "node_id_1"
              shift = shoot_struct%shift_m_node_surf(node_id_1)
              intersect_1(1:nb_surface_1) = shoot_struct%m_node_surf( shift+1:shift+nb_surface_1 )
              ! ------

              ! ------
              ! 2nd node
              node_id_2 = intbuf_tab(nin)%irectm(4*(segment_id-1)+egde_list(2,iedge))
              nb_surface_2 = shoot_struct%shift_m_node_surf(node_id_2+1) - shoot_struct%shift_m_node_surf(node_id_2)   ! get the number of surface for the node "node_id_2"
              shift = shoot_struct%shift_m_node_surf(node_id_2)
              intersect_2(1:nb_surface_2) = shoot_struct%m_node_surf( shift+1:shift+nb_surface_2 )
              ! ------

              if(nb_surface_1>0.and.nb_surface_2>0) then
                call intersect_2_sorted_sets( intersect_1,nb_surface_1, &
                                              intersect_2,nb_surface_2, &
                                              result_intersect_0,nb_result_intersect_0 )
              else
                nb_result_intersect_0 = 0
              endif
              ! end : intersection of segment 
              ! ---------------

              ! ---------------  
              ! only consider the segments of the interface NIN          
              allocate( n_normal(3,nb_result_intersect_0) )
              allocate( n_iedge(nb_result_intersect_0) )
              allocate( my_reduced_list(nb_result_intersect_0,2) )
              allocate( my_reduced_neighbour(nb_result_intersect_0,4) )

              call get_segment_interface_id( ninter,nb_result_intersect_0,result_intersect_0, &
                                             nin,my_reduced_nb,my_reduced_list,my_reduced_neighbour, &
                                             shoot_struct,intbuf_tab)

              do ijk=1,my_reduced_nb
                ! segment/surface orientation
                n_segment_id = my_reduced_list(ijk,1) ! connected segment id
                call get_segment_orientation( n_segment_id,s_elem_state,nixs,numels,numnod, &
                                            elem_state,ixs,x,intbuf_tab(nin) )
                ! compute the normal to the segment "n_segment_id"
                call get_segment_normal( n_segment_id,segment_node_id,segment_position,n_normal(1,ijk),intbuf_tab(nin),numnod,x )
                ! find the edge id of n_segment_id
                call get_segment_edge( n_segment_id,node_id_1,node_id_2,n_iedge(ijk),intbuf_tab(nin) )
              enddo
              ! ---------------  

              ! ---------------    
              ! intersection of processor list
              ! ------
              ! 1srt node
              shift = shoot_struct%shift_m_node_proc(node_id_1)
              nb_proc_1 = shoot_struct%shift_m_node_proc(node_id_1+1) - shoot_struct%shift_m_node_proc(node_id_1) ! get the number of processor of the node "node_id_1"  
              intersect_3(1:nb_proc_1) = shoot_struct%m_node_proc( shift+1:shift+nb_proc_1 )
              ! ------

              ! ------
              ! 2nd node
              nb_proc_2 = shoot_struct%shift_m_node_proc(node_id_2+1) - shoot_struct%shift_m_node_proc(node_id_2) ! get the number of processor of the node "node_id_2"
              ! ------
              if(nb_proc_1>=1.and.nb_proc_2>=1) then
                shift = shoot_struct%shift_m_node_proc(node_id_2)
                intersect_4(1:nb_proc_2) = shoot_struct%m_node_proc( shift+1:shift+nb_proc_2 )

                call intersect_2_sorted_sets( intersect_3,nb_proc_1, &
                                             intersect_4,nb_proc_2, &
                                             result_intersect_2,nb_result_intersect_2 )
              elseif(nb_proc_2<1.or.nb_proc_1<1) then
                ! this case is not possible, i hope i'm not here :)
              else
                nb_result_intersect_2 = 0
              endif

              ! -----------
              ! loop over the proc with "node_id_1" and "node_id_2"
              do j=1,nb_result_intersect_2
                proc_id = result_intersect_2(j) ! get the processor id
                ! -----------
                ! check if the size is enough
                my_size = 8 + 10*my_reduced_nb + nb_result_intersect_2 +  3 ! get the mpi buffer size for the current new segment
                if(my_address_proc(proc_id)+my_size>s_buffer(proc_id)%size_my_real_array_1d) then
                  old_size = s_buffer(proc_id)%size_my_real_array_1d
                  allocate( my_real_tmp_array(old_size) )
                  my_real_tmp_array(1:old_size) = s_buffer(proc_id)%my_real_array_1d(1:old_size)
                  call dealloc_my_real_1d_array(s_buffer(proc_id))
                  s_buffer(proc_id)%size_my_real_array_1d = 2*(s_buffer(proc_id)%size_my_real_array_1d + my_size) + 1
                  call alloc_my_real_1d_array(s_buffer(proc_id))
                  s_buffer(proc_id)%my_real_array_1d(1:old_size) = my_real_tmp_array(1:old_size)
                  deallocate( my_real_tmp_array )
                endif
                ! -----------

                ! -----------
                ! save the segment data for the mpi comm

                my_offset = my_address_proc(proc_id)
                my_integer = -intbuf_tab(nin)%mseglo(segment_id)
                s_buffer(proc_id)%my_real_array_1d(my_offset+1) = transfer(my_integer,my_real_variable) ! global segment id

                my_integer = segment_id
                s_buffer(proc_id)%my_real_array_1d(my_offset+2) = transfer(my_integer,my_real_variable) ! local segment id


                my_integer = itab(node_id_1)
                s_buffer(proc_id)%my_real_array_1d(my_offset+3) = transfer(my_integer,my_real_variable) ! node id 
                my_integer = itab(node_id_2)
                s_buffer(proc_id)%my_real_array_1d(my_offset+4) = transfer(my_integer,my_real_variable) ! node id 

                my_integer = nin
                s_buffer(proc_id)%my_real_array_1d(my_offset+5) = transfer(my_integer,my_real_variable) ! interface id

                my_iedge = iedge
                if(iedge==3.and.edge_number==3) my_iedge = 4
                my_integer = my_iedge
                s_buffer(proc_id)%my_real_array_1d(my_offset+6) = transfer(my_integer,my_real_variable)

                my_integer = my_reduced_nb
                s_buffer(proc_id)%my_real_array_1d(my_offset+7) = transfer(my_integer,my_real_variable)

                my_integer = nb_result_intersect_2
                s_buffer(proc_id)%my_real_array_1d(my_offset+8) = transfer(my_integer,my_real_variable)               

                my_offset = my_offset+8

                do ijk=1,my_reduced_nb
                  my_integer = -intbuf_tab(nin)%mseglo(my_reduced_list(ijk,1))
                  s_buffer(proc_id)%my_real_array_1d(my_offset+ijk) = transfer(my_integer,my_real_variable)                  
                enddo
                my_offset = my_offset+my_reduced_nb

                do ijk=1,my_reduced_nb
                  my_integer = my_reduced_list(ijk,1)
                  s_buffer(proc_id)%my_real_array_1d(my_offset+ijk) = transfer(my_integer,my_real_variable)                  
                enddo
                my_offset = my_offset+my_reduced_nb

                s_buffer(proc_id)%my_real_array_1d(my_offset+1) = normal(1,i) ! new segment normal (x)
                s_buffer(proc_id)%my_real_array_1d(my_offset+2) = normal(2,i) ! new segment normal (y)
                s_buffer(proc_id)%my_real_array_1d(my_offset+3) = normal(3,i) ! new segment normal (z)
                my_offset = my_offset+3
                do ijk=1,my_reduced_nb
                  s_buffer(proc_id)%my_real_array_1d(my_offset+1) = n_normal(1,ijk) ! neighbour segment normal (x)
                  s_buffer(proc_id)%my_real_array_1d(my_offset+2) = n_normal(2,ijk) ! neighbour segment normal (y)
                  s_buffer(proc_id)%my_real_array_1d(my_offset+3) = n_normal(3,ijk) ! neighbour segment normal (z)    
                  my_offset = my_offset + 3       
                enddo

                do ijk=1,my_reduced_nb
                  my_integer = n_iedge(ijk)
                  s_buffer(proc_id)%my_real_array_1d(my_offset+ijk) = transfer(my_integer,my_real_variable) ! edge id
                enddo
                my_offset = my_offset + my_reduced_nb 
                do ijk=1,nb_result_intersect_2
                  my_integer = result_intersect_2(ijk) ! processor id
                  s_buffer(proc_id)%my_real_array_1d(my_offset+ijk) = transfer(my_integer,my_real_variable)
                enddo
                my_offset = my_offset + nb_result_intersect_2 

                do ijk=1,my_reduced_nb
                  do kji=1,4
                    my_integer = my_reduced_neighbour(ijk,kji)
                    s_buffer(proc_id)%my_real_array_1d(my_offset+kji) = transfer(my_integer,my_real_variable) ! boolean
                  enddo
                    my_offset = my_offset + 4
                enddo

                my_address_proc(proc_id) = my_offset ! address for the next new segment

                s_buffer_size(1,proc_id) = my_offset ! size of mpi buffer (send)
                s_buffer_size(2,proc_id) = s_buffer_size(2,proc_id) + 1 ! number of segment
              enddo
              deallocate( n_normal )
              deallocate( n_iedge )
              deallocate( my_reduced_list )
              deallocate( my_reduced_neighbour )
              ! -----------

            enddo
            ! end : loop over the edge
            ! ------------------------
          enddo
          ! end : loop over the new active segment/surface
          ! --------------------------

          ! --------------------------
          ! exchange of data : local proc --> remote proc  - new segment id + list of local connected segment
          ! remote proc checks if there are some remote connected segments
          ! exchange of data : remote proc --> local proc + other remote proc - new segment id + list of remote connected segment 
          call spmd_exch_neighbour_segment(nspmd,ispmd, &
                                           ninter,numnod,nixs,numels,s_elem_state, &
                                           s_buffer_size,r_buffer_size,s_buffer_2_size,r_buffer_2_size,&
                                           iad_elem,itabm1,ixs,elem_state,x, &
                                           s_buffer,r_buffer,s_buffer_2,r_buffer_2, &
                                           intbuf_tab,shoot_struct)
          ! --------------------------

          ! --------------------------
          ! everybody (local proc + remote proc) : criteria computation + choose a neighbour
          call update_neighbour_segment( ispmd,nspmd,ninter,r_buffer_size,r_buffer_2_size,  &
                                         r_buffer,r_buffer_2,intbuf_tab)
          ! --------------------------

          ! --------------------------
          ! exchange between processor to update the frontier 
          if(nspmd>1) call spmd_update_frontier_int25( ispmd,nspmd,ninter25,npari,ninter,nbintc, &
                                                       numnod,nbddedgt,nbddedg_max, &
                                                       ipari,intlist,itab,  &
                                                       intbuf_tab,spmd_arrays )
          ! --------------------------

          ! --------------------------
          ! working array : surface
          deallocate( result_intersect_0 )
          deallocate( intersect_1 )
          deallocate( intersect_2 )
          deallocate( index_neighbour )
          ! working array : processor
          deallocate( result_intersect_2 )
          deallocate( intersect_3 )
          deallocate( intersect_4 )
         
          deallocate( angle_min )
          deallocate( iedge_min )
          deallocate( segment_id_min )
          deallocate( normal )
          ! mpi buffer 
          do i=1,nspmd
            call dealloc_my_real_1d_array(s_buffer(i))
            if(r_buffer(i)%size_my_real_array_1d>0) then
              call dealloc_my_real_1d_array(r_buffer(i))
            endif
            if(s_buffer_2(i)%size_my_real_array_1d>0) then
              call dealloc_my_real_1d_array(s_buffer_2(i))
            endif
            if(r_buffer_2(i)%size_my_real_array_1d>0) then
              call dealloc_my_real_1d_array(r_buffer_2(i))
            endif
          enddo
          deallocate( s_buffer )
          deallocate( r_buffer )
          deallocate( s_buffer_2 )
          deallocate( r_buffer_2 )
          ! --------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine get_neighbour_surface
      end module get_neighbour_surface_mod
