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
!||    get_neighbour_surface_mod   ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||--- called by ------------------------------------------------------
!||    resol                       ../engine/source/engine/resol.F
!||====================================================================
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
!||====================================================================
!||    get_neighbour_surface             ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||--- called by ------------------------------------------------------
!||    resol                             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    alloc_my_real_1d_array            ../common_source/modules/array_mod.F
!||    dealloc_my_real_1d_array          ../common_source/modules/array_mod.F
!||    get_convexity_normals             ../engine/source/interfaces/interf/get_convexity_normals.F90
!||    get_segment_interface_id          ../engine/source/interfaces/interf/get_segment_interface_id.F90
!||    get_segment_normal                ../engine/source/interfaces/interf/get_segment_normal.F90
!||    get_segment_orientation           ../engine/source/interfaces/interf/get_segment_orientation.F90
!||    spmd_exch_neighbour_segment       ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||    spmd_update_frontier_int25        ../engine/source/mpi/interfaces/spmd_update_frontier_int25.F90
!||    update_neighbour_segment          ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||--- uses       -----------------------------------------------------
!||    array_mod                         ../common_source/modules/array_mod.F
!||    constant_mod                      ../common_source/modules/constant_mod.F
!||    get_convexity_normals_mod         ../engine/source/interfaces/interf/get_convexity_normals.F90
!||    get_segment_criteria_mod          ../engine/source/interfaces/interf/get_segment_criteria.F90
!||    get_segment_interface_id_mod      ../engine/source/interfaces/interf/get_segment_interface_id.F90
!||    get_segment_normal_mod            ../engine/source/interfaces/interf/get_segment_normal.F90
!||    get_segment_orientation_mod       ../engine/source/interfaces/interf/get_segment_orientation.F90
!||    intbufdef_mod                     ../common_source/modules/interfaces/intbufdef_mod.F90
!||    nodal_arrays_mod                  ../common_source/modules/nodal_arrays.F90
!||    precision_mod                     ../common_source/modules/precision_mod.F90
!||    shooting_node_mod                 ../engine/share/modules/shooting_node_mod.F
!||    spmd_arrays_mod                   ../common_source/modules/interfaces/spmd_arrays_mod.F
!||    spmd_exch_neighbour_segment_mod   ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||    spmd_update_frontier_int25_mod    ../engine/source/mpi/interfaces/spmd_update_frontier_int25.F90
!||    update_neighbour_segment_mod      ../engine/source/interfaces/interf/update_neighbour_segment.F90
!||====================================================================
        subroutine get_neighbour_surface( ispmd,nspmd,ninter25,npari,ninter,  &
                                          nbintc,nixs,nixc,nixtg,numnod,  &
                                          numels,numelc,numeltrg,s_elem_state, &
                                          nbddedgt,nbddedg_max,  &
                                          elem_state,ipari,intlist,nodes, &
                                          newfront,ixs,ixc,ixtg,  &
                                          iad_elem,x,         &
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
          use get_convexity_normals_mod , only : get_convexity_normals
          use get_segment_orientation_mod , only : get_segment_orientation
          use get_segment_criteria_mod , only : get_segment_criteria
          use get_segment_interface_id_mod , only : get_segment_interface_id
          use spmd_update_frontier_int25_mod , only : spmd_update_frontier_int25
          use spmd_exch_neighbour_segment_mod , only : spmd_exch_neighbour_segment
          use update_neighbour_segment_mod , only : update_neighbour_segment
          use spmd_arrays_mod , only : spmd_arrays_
          use nodal_arrays_mod, only : nodal_arrays_
          use precision_mod, only : WP 
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
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
          integer, intent(in) :: nixc !< 1rst dim of "ixc" array
          integer, intent(in) :: nixtg !< 1rst dim of "ixtg" array
          integer, intent(in) :: numels !< number of solid element
          integer, intent(in) :: numelc !< number of shell element
          integer, intent(in) :: numeltrg !< number of shell3n element
          integer, intent(in) :: numnod !< total number of node
          integer, intent(in) :: s_elem_state !< dim of elem_state
          integer, intent(inout) :: nbddedgt !< number of frontier edges
          integer, intent(inout) :: nbddedg_max !< number of frontier edges
          logical, dimension(s_elem_state), intent(in) :: elem_state !< state of the element : on or off
          integer, dimension(npari,ninter), intent(in) :: ipari !< interface data
          integer, dimension(nbintc), intent(in) :: intlist
          type(nodal_arrays_) :: nodes !< nodal arrays                                                                              
          integer, dimension(ninter),intent(inout) :: newfront !< flag to force some exchanges related to S nodes between processor (if a S node becomes a shooting node - all interface) / force the collision detection algo if a new segment is activated for the (interface 25 + solid erosion)
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid element data
          integer, dimension(nixc,numelc), intent(in) :: ixc !< shell element data
          integer, dimension(nixtg,numeltrg), intent(in) :: ixtg !< shell3n element data
          integer, dimension(2,nspmd+1), intent(in) :: iad_elem !< frontier between processor
          real(kind=WP), dimension(3,numnod), intent(in) :: x !< nodal position
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab    !< interface data 
          type(spmd_arrays_), intent(inout) :: spmd_arrays !< structure for interface spmd arrays
          type(shooting_node_type), intent(inout) :: shoot_struct !< structure for shooting node algo
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,ijk,kji,ni,nrtm,idel25_solid,nty,mvoisin_change,itria
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
          integer, dimension(2,4), parameter :: egde_list = reshape( (/1,2,2,3,3,4,4,1/) , shape(egde_list) )
          integer, dimension(4) :: segment_node_id
          integer, dimension(:), allocatable :: n_iedge
          integer, dimension(:,:), allocatable :: my_reduced_list,my_reduced_neighbour
          integer, dimension(2,nspmd) :: s_buffer_size,r_buffer_size ! size of s/r buffer
          integer, dimension(3,nspmd) :: s_buffer_2_size,r_buffer_2_size ! size of s/r buffer
#ifdef MYREAL8
          integer(kind=8) :: my_integer
#else
          integer(kind=4) :: my_integer
#endif
          real(kind=WP) :: my_real_variable
          real(kind=WP), dimension(:,:), allocatable :: n_normal, n_vconvexity
          real(kind=WP), dimension(3) :: segment_position
          integer, dimension(:), allocatable :: result_intersect_0,result_intersect_1,result_intersect_2
          integer, dimension(:), allocatable :: intersect_1,intersect_2
          integer, dimension(:), allocatable :: intersect_4,intersect_3
          integer, dimension(:,:), allocatable :: index_neighbour
!          integer, dimension(:,:,:), allocatable :: iedge_min,segment_id_min
          real(kind=WP), dimension(:), allocatable :: my_real_tmp_array
!          real(kind=WP), dimension(:,:), allocatable :: angle_min
          real(kind=WP), dimension(3) :: normal , v_convexity 
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

          normal(1:3) = zero
          v_convexity(1:3) = zero
          ! mpi buffer 
          allocate( s_buffer(nspmd) )
          allocate( r_buffer(nspmd) )
          allocate( s_buffer_2(nspmd) )
          allocate( r_buffer_2(nspmd) )
          do i=1,nspmd
            s_buffer(i)%size_my_real_array_1d = 13*shoot_struct%max_surf_nb
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
            call get_segment_orientation( segment_id,s_elem_state,nixs,nixc,nixtg, &
                                          numels,numelc,numeltrg,numnod, &
                                          elem_state,ixs,ixc,ixtg,x, &
                                          intbuf_tab(nin),shoot_struct )
          enddo

          ! --------------------------
          ! loop over all segment/surface without neighbours

          ! 
          edge_number = 4
          do ni =1,nbintc
            nin = intlist(ni)
            nty=IPARI(7,nin)
            idel25_solid = ipari(100,nin)
            nrtm = ipari(4,nin)
            if(nty==25.AND.idel25_solid > 0) THEN
              do segment_id=1,nrtm

                if(intbuf_tab(nin)%stfm(segment_id) > zero.and.intbuf_tab(nin)%ielem_m(2*(segment_id-1)+1) <= numels) THEN

                  itria = 0
                  node_id_3 = intbuf_tab(nin)%irectm(4*(segment_id-1)+3)
                  node_id_4 = intbuf_tab(nin)%irectm(4*(segment_id-1)+4)
                  if( node_id_3==node_id_4 ) itria = 1
   
                  mvoisin_change = 0
                  do iedge=1,edge_number
                    if(itria==0.or.iedge/=3) then
                       if (intbuf_tab(nin)%mvoisin(4*(segment_id-1)+iedge)==0) mvoisin_change = 1
                    endif
                  enddo
                  if(mvoisin_change==1) THEN


                  ! compute the normal to the segment "segment_id"
                   normal(1:3) = zero
                   call get_segment_normal( segment_id,segment_node_id,segment_position,normal,intbuf_tab(nin),numnod,x )

        
                  ! check if the segment has 3 or 4 nodes (triangle or quadrangle)
                    my_address = 0

                  ! --------------------------
            ! loop over the edge of the segment
                  do iedge = 1, edge_number
                   if(itria==0.or.iedge/=3) then

                    if (intbuf_tab(nin)%mvoisin(4*(segment_id-1)+iedge)==0) then

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
                      allocate( n_vconvexity(3,nb_result_intersect_0) )
                      allocate( n_iedge(nb_result_intersect_0) )
                      allocate( my_reduced_list(nb_result_intersect_0,2) )
                      allocate( my_reduced_neighbour(nb_result_intersect_0,4) )

                      call get_segment_interface_id( ninter,nb_result_intersect_0,result_intersect_0, &
                                                     nin,my_reduced_nb,my_reduced_list,my_reduced_neighbour, &
                                                     shoot_struct,intbuf_tab,node_id_1,node_id_2,n_iedge)

                      ! compute the tangent vector to the segment around the edge "segment_id" 
                      call get_convexity_normals( node_id_1,node_id_2,normal,v_convexity,numnod,x )


                      do ijk=1,my_reduced_nb
                       ! segment/surface orientation
                       n_segment_id = my_reduced_list(ijk,1) ! connected segment id
                       ! compute the normal to the segment "n_segment_id"
                       call get_segment_normal( n_segment_id,segment_node_id,segment_position,n_normal(1,ijk), &
                                                intbuf_tab(nin),numnod,x )
                       ! compute the tangent vector to the segment around the edge "n_segment_id" 
                       call get_convexity_normals( node_id_1,node_id_2,n_normal(1,ijk),n_vconvexity(1,ijk),numnod,x )
                      enddo
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
                        my_size = 8 + 13*my_reduced_nb + nb_result_intersect_2 +  3 + 3! get the mpi buffer size for the current new segment
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


                       my_integer = nodes%itab(node_id_1)
                       s_buffer(proc_id)%my_real_array_1d(my_offset+3) = transfer(my_integer,my_real_variable) ! node id 
                       my_integer = nodes%itab(node_id_2)
                       s_buffer(proc_id)%my_real_array_1d(my_offset+4) = transfer(my_integer,my_real_variable) ! node id 

                       my_integer = nin
                       s_buffer(proc_id)%my_real_array_1d(my_offset+5) = transfer(my_integer,my_real_variable) ! interface id

                       my_iedge = iedge
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

                       s_buffer(proc_id)%my_real_array_1d(my_offset+1) = normal(1) ! new segment normal (x)
                       s_buffer(proc_id)%my_real_array_1d(my_offset+2) = normal(2) ! new segment normal (y)
                       s_buffer(proc_id)%my_real_array_1d(my_offset+3) = normal(3) ! new segment normal (z)
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

                       s_buffer(proc_id)%my_real_array_1d(my_offset+1) = v_convexity(1) ! new segment normal (x)
                       s_buffer(proc_id)%my_real_array_1d(my_offset+2) = v_convexity(2) ! new segment normal (y)
                       s_buffer(proc_id)%my_real_array_1d(my_offset+3) = v_convexity(3) ! new segment normal (z)
                       my_offset = my_offset+3
                       do ijk=1,my_reduced_nb
                         s_buffer(proc_id)%my_real_array_1d(my_offset+1) = n_vconvexity(1,ijk) ! neighbour segment normal (x)
                         s_buffer(proc_id)%my_real_array_1d(my_offset+2) = n_vconvexity(2,ijk) ! neighbour segment normal (y)
                         s_buffer(proc_id)%my_real_array_1d(my_offset+3) = n_vconvexity(3,ijk) ! neighbour segment normal (z)    
                         my_offset = my_offset + 3        
                       enddo

                       my_address_proc(proc_id) = my_offset ! address for the next new segment

                       s_buffer_size(1,proc_id) = my_offset ! size of mpi buffer (send)
                       s_buffer_size(2,proc_id) = s_buffer_size(2,proc_id) + 1 ! number of segment
                      enddo
                      deallocate( n_normal )
                      deallocate( n_iedge )
                      deallocate( my_reduced_list )
                      deallocate( my_reduced_neighbour )
                      deallocate( n_vconvexity )
                      ! -----------
                    endif
                   endif
                  enddo
            ! end : loop over the edge
            ! ------------------------
                endif
               endif
              enddo
          ! end : loop over the new active segment/surface
          ! --------------------------
             endif

          ENDDO

!          endif
          ! --------------------------

          ! --------------------------
          ! exchange of data : local proc --> remote proc  - new segment id + list of local connected segment
          ! remote proc checks if there are some remote connected segments
          ! exchange of data : remote proc --> local proc + other remote proc - new segment id + list of remote connected segment 
          call spmd_exch_neighbour_segment(nspmd,ispmd, &
                                           ninter,numnod, &
                                           s_buffer_size,r_buffer_size,s_buffer_2_size,r_buffer_2_size,&
                                           iad_elem,nodes,x, &
                                           s_buffer,r_buffer,s_buffer_2,r_buffer_2, &
                                           intbuf_tab,shoot_struct)
          ! --------------------------

          ! --------------------------
          ! everybody (local proc + remote proc) : criteria computation + choose a neighbour
          call update_neighbour_segment( ispmd,nspmd,ninter,r_buffer_size,r_buffer_2_size,  &
                                         r_buffer,r_buffer_2,intbuf_tab,shoot_struct)
          ! --------------------------

          ! --------------------------
          ! exchange between processor to update the frontier 
          if(nspmd>1) call spmd_update_frontier_int25( ispmd,nspmd,ninter25,npari,ninter,nbintc, &
                                                       numnod,nbddedgt,nbddedg_max, &
                                                       ipari,intlist,nodes%itab,  &
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
