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
      module update_neighbour_segment_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine finds the neighbour segment of a new active segments
!! \details for each new active segment of an interface /TYPE25 and for the 4 edges of the segment :
!!          each processor with at least 1 local or remote connected segments computes the following operations:
!!          * loop over the local connected segment to compute the criteria for the pair (new active segment/local segment)
!!          * loop over the remote connected segment to compute the criteria for the pair (new active segment/remote segment)
!!          * choose a neighbour (neighbour = segment with the lowest criteria)
!!          to insure the parith/on, the list of new active segment is sorted (ascending order)
        subroutine update_neighbour_segment( ispmd,nspmd,ninter,r_buffer_size,r_buffer_2_size,r_buffer,r_buffer_2,intbuf_tab)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use intbufdef_mod , only : intbuf_struct_
          use array_mod , only : array_type
          use get_segment_criteria_mod , only : get_segment_criteria
          use constant_mod , only : ep30
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
          integer, intent(in) :: nspmd !< number of mpi tasks
          integer, intent(in) :: ninter !< number of interface
          integer, dimension(2,nspmd), intent(in) :: r_buffer_size
          integer, dimension(3,nspmd), intent(in) :: r_buffer_2_size
          type(array_type), dimension(nspmd), intent(in) :: r_buffer
          type(array_type), dimension(nspmd), intent(in) :: r_buffer_2
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,ijk,kji
          integer :: nin
          integer :: nb_new_segment,total_nb_segment,local_nb_new_segment
          integer :: next_segment
          integer :: nb_connected_segment,nb_r_connected_segment
          integer :: proc_number,proc_id,r_proc_id,n_proc_id
          integer :: my_offset,my_offset_0,my_offset_1
          integer :: my_offset_2,my_offset_3,my_offset_4
          integer :: my_offset_5,my_offset_6
          integer :: segment_hash_id
          integer :: ierror


          integer :: segment_id,segment_id_2,local_segment_id,n_segment_id,local_n_segment_id
          integer :: my_iedge,my_iedge_2,n_iedge_id,previous_iedge_id
          integer :: my_id
          integer :: seg_id,n_seg_id,previous_n_seg_id
          integer :: address,r_address
          integer :: already_a_neighbour
          my_real :: my_criteria
          my_real, dimension(3) :: normal,n_normal

#ifdef MYREAL8
          integer(kind=8) :: my_integer
          integer(kind=8) :: my_int_variable   
#else
          integer(kind=4) :: my_integer
          integer(kind=4) :: my_int_variable   
#endif
          integer, dimension(:), allocatable :: new_segment_id,permutation
          integer, dimension(:,:), allocatable :: list_new_segment
          integer, dimension(:,:,:), allocatable :: segment_pair
          my_real, dimension(:,:), allocatable :: criteria

! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          ! ------------
          ! get the total number of new segment
          nb_new_segment = 0
          do i =1,nspmd
            nb_new_segment = nb_new_segment + r_buffer_size(2,i)
          enddo
          ! ------------

          ! ------------
          ! allocate some arrays + initialization
          allocate( list_new_segment(nb_new_segment,6) )
          list_new_segment(1:nb_new_segment,1:6) = 0
          allocate( new_segment_id(nb_new_segment) )
          allocate( permutation(nb_new_segment) )
          new_segment_id(1:nb_new_segment) = 0
          permutation(1:nb_new_segment) = 0
          ! ------------

          ! ------------
          ! find the new segment data (processor / address in the r_buffer / global segment id / 2 global node ids
          total_nb_segment = nb_new_segment ! count the total number of segment (new segment/local connected segment/remote connected segment)
          my_offset = 0
          do i =1,nspmd
            local_nb_new_segment = r_buffer_size(2,i)
            if(local_nb_new_segment>0) then
              next_segment = 0
              do j=1,local_nb_new_segment
                my_offset = my_offset + 1
                list_new_segment(my_offset,1) = i ! save the proc id
                list_new_segment(my_offset,2) = next_segment ! save the address
                my_integer = transfer(r_buffer(i)%my_real_array_1d(next_segment + 1),my_int_variable) ! get the global segment id
                list_new_segment(my_offset,3) = abs(my_integer) ! save the global segment id
                my_integer = transfer(r_buffer(i)%my_real_array_1d(next_segment+6),my_int_variable) ! get the edge
                list_new_segment(my_offset,4) = my_integer ! save the edge

                my_integer = transfer(r_buffer(i)%my_real_array_1d(next_segment+5),my_int_variable) ! get the interface id
                list_new_segment(my_offset,5) = my_integer ! save the interface id

                my_integer = transfer(r_buffer(i)%my_real_array_1d(next_segment + 2),my_int_variable) ! get the local segment id
                list_new_segment(my_offset,6) = my_integer ! save the local segment id

                my_integer = transfer(r_buffer(i)%my_real_array_1d(next_segment + 7),my_int_variable) ! get the number of r connected segment
                nb_connected_segment = my_integer
                my_integer = transfer(r_buffer(i)%my_real_array_1d(next_segment + 8),my_int_variable) ! get the number of procs with the 2 nodes "node_id_1" & "node_id_2"
                proc_number = my_integer

                next_segment = next_segment + 8 + 10*nb_connected_segment+ 3 + proc_number

                total_nb_segment = total_nb_segment + nb_connected_segment ! add the local segment number
              enddo

            endif
          enddo

          do i =1,nspmd
            total_nb_segment = total_nb_segment + r_buffer_2_size(2,i) ! add the remote segment number
          enddo
          ! ------------
          
          ! ------------
          ! sort the new segment id (ascending order)
          new_segment_id(1:nb_new_segment) = list_new_segment(1:nb_new_segment,3)
          call myqsort_int(nb_new_segment,new_segment_id,permutation,ierror)
          ! ------------

          ! ------------
          ! hash initialization
          segment_hash_id = 666
          call c_new_hash( segment_hash_id, total_nb_segment )
          
          seg_id = 0
          do i=1,nb_new_segment
            segment_id = list_new_segment(permutation(i),3)
            ierror = -1
            call c_hash_find( segment_hash_id,segment_id,ierror ) ! check if "segment id" is already in the hash table

            if(ierror==-1) then  ! no --> need to add it
              seg_id = seg_id + 1
              call c_hash_insert( segment_hash_id,segment_id,seg_id )
              new_segment_id(i) = seg_id
            else
              new_segment_id(i) = ierror
            endif
          enddo
          ! ------------

          allocate( segment_pair(total_nb_segment,4,5) )
          allocate( criteria(total_nb_segment,4) )
          segment_pair(1:total_nb_segment,1:4,1:5) = 0
          criteria(1:nb_new_segment,1:4) = EP30

          ! --------------------------
          ! loop over the new active segment
          !   for each new active segment :
          !     * loop the local segment to compute the criteria (r_buffer)
          !     * loop the remote segment to compute the criteria (r_buffer_2)
          seg_id = nb_new_segment
          do i=1,nb_new_segment
            segment_id = list_new_segment(permutation(i),3) ! get the global segment id
            local_segment_id = list_new_segment(permutation(i),6) ! get the local segment id
            proc_id = list_new_segment(permutation(i),1) ! get the proc id
            address = list_new_segment(permutation(i),2) ! get the address in the r_buffer
            nin = list_new_segment(permutation(i),5) ! get the interface id

            my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(address + 7),my_int_variable) ! get the number of r connected segment
            nb_connected_segment = my_integer
            my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(address + 6),my_int_variable) ! get the edge id of the new segment
            my_iedge = my_integer
            ! get the normal of the new segment
            my_offset_0 = address + 8+2*nb_connected_segment
            normal(1:3) = r_buffer(proc_id)%my_real_array_1d(my_offset_0+1:my_offset_0+3)

            my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(address + 8),my_int_variable) ! get the number of proc
            proc_number = my_integer

            my_offset_1 = address + 8
            my_offset_2 = address + 8+nb_connected_segment
            my_offset_3 = address + 8+2*nb_connected_segment+3
            my_offset_4 = address + 8+5*nb_connected_segment+3
            my_offset_5 = address + 8+6*nb_connected_segment+3
            my_offset_6 = address + 8+6*nb_connected_segment+3+proc_number

            ! ---------------------
            ! local neighbour segments (=same processor than "segment_id"'s processor)
            do j=1,nb_connected_segment
              my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(my_offset_1 + j),my_int_variable) ! get global neighbour segment id
              n_segment_id = abs(my_integer)
!(8+5*number of connected segment+3:8+6*number of connected segment+3)
              my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(my_offset_4 + j),my_int_variable) ! get edge id of the neighbour segment
              n_iedge_id = my_integer
              my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(my_offset_2 + j),my_int_variable) ! get local neighbour segment id
              local_n_segment_id = abs(my_integer)
              ierror = -1
              call c_hash_find( segment_hash_id,n_segment_id,ierror ) ! check if "n_segment id" is already in the hash table

              if(ierror==-1) then  ! no --> need to add it
                seg_id = seg_id + 1
                call c_hash_insert( segment_hash_id,n_segment_id,seg_id )
                n_seg_id = seg_id
                do kji=1,4
                  my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(my_offset_6 + 4*(j-1) + kji),my_int_variable) ! check if the n_segment has already a neighbour
                  already_a_neighbour = my_integer
                  segment_pair(n_seg_id,kji,1) = already_a_neighbour
                enddo
              else
                n_seg_id = ierror
              endif

              n_normal(1) = r_buffer(proc_id)%my_real_array_1d(my_offset_3 + 3*(j-1)+1)
              n_normal(2) = r_buffer(proc_id)%my_real_array_1d(my_offset_3 + 3*(j-1)+2)
              n_normal(3) = r_buffer(proc_id)%my_real_array_1d(my_offset_3 + 3*(j-1)+3)
              my_criteria = EP30

              ! -------
              if(segment_id/=n_segment_id) then
                call get_segment_criteria( my_criteria,normal,n_normal )

                if(my_criteria<criteria(new_segment_id(i),my_iedge).and.segment_pair(n_seg_id,n_iedge_id,1)==0) then

                  criteria(new_segment_id(i),my_iedge) = my_criteria
                  criteria(n_seg_id,n_iedge_id) = my_criteria

                  previous_n_seg_id = segment_pair(new_segment_id(i),my_iedge,3) ! get the previous local segment id 
                  previous_iedge_id = segment_pair(new_segment_id(i),my_iedge,4) ! get the previous edge id

                  segment_pair(new_segment_id(i),my_iedge,1) = n_segment_id
                  segment_pair(new_segment_id(i),my_iedge,2) = proc_id
                  segment_pair(new_segment_id(i),my_iedge,3) = n_seg_id
                  segment_pair(new_segment_id(i),my_iedge,4) = n_iedge_id
                  segment_pair(new_segment_id(i),my_iedge,5) = local_n_segment_id
!
                  segment_pair(n_seg_id,n_iedge_id,1) = segment_id
                  segment_pair(n_seg_id,n_iedge_id,2) = proc_id
                  segment_pair(n_seg_id,n_iedge_id,3) = new_segment_id(i)
                  segment_pair(n_seg_id,n_iedge_id,4) = my_iedge
                  segment_pair(n_seg_id,n_iedge_id,5) = local_segment_id

                  if(previous_n_seg_id/=0) then
                    segment_pair(previous_n_seg_id,previous_iedge_id,1) = 0
                    segment_pair(previous_n_seg_id,previous_iedge_id,2) = 0
                    segment_pair(previous_n_seg_id,previous_iedge_id,3) = 0
                    segment_pair(previous_n_seg_id,previous_iedge_id,4) = 0
                    segment_pair(previous_n_seg_id,previous_iedge_id,5) = 0
                  endif
                elseif(my_criteria==criteria(new_segment_id(i),my_iedge).and.segment_pair(n_seg_id,n_iedge_id,1)==0) then
                  if(n_segment_id<segment_pair(new_segment_id(i),my_iedge,1)) then

                    criteria(new_segment_id(i),my_iedge) = my_criteria
                    criteria(n_seg_id,n_iedge_id) = my_criteria

                    previous_n_seg_id = segment_pair(new_segment_id(i),my_iedge,3) ! get the previous local segment id 
                    previous_iedge_id = segment_pair(new_segment_id(i),my_iedge,4) ! get the previous edge id

                    segment_pair(new_segment_id(i),my_iedge,1) = n_segment_id
                    segment_pair(new_segment_id(i),my_iedge,2) = proc_id
                    segment_pair(new_segment_id(i),my_iedge,3) = n_seg_id
                    segment_pair(new_segment_id(i),my_iedge,4) = n_iedge_id
                    segment_pair(new_segment_id(i),my_iedge,5) = local_n_segment_id

                    segment_pair(n_seg_id,n_iedge_id,1) = segment_id
                    segment_pair(n_seg_id,n_iedge_id,2) = proc_id
                    segment_pair(n_seg_id,n_iedge_id,3) = new_segment_id(i)
                    segment_pair(n_seg_id,n_iedge_id,4) = my_iedge
                    segment_pair(n_seg_id,my_iedge,5) = local_segment_id

                    if(previous_n_seg_id/=0) then
                      segment_pair(previous_n_seg_id,previous_iedge_id,1) = 0
                      segment_pair(previous_n_seg_id,previous_iedge_id,2) = 0
                      segment_pair(previous_n_seg_id,previous_iedge_id,3) = 0
                      segment_pair(previous_n_seg_id,previous_iedge_id,4) = 0
                      segment_pair(previous_n_seg_id,previous_iedge_id,5) = 0
                    endif
                  endif
                endif
              endif
              ! -------
            enddo
            ! ---------------------


            ! ---------------------
            ! remote neighbour segments (=different processor than "segment_id"'s processor)
            do k=1,proc_number

              my_integer = transfer(r_buffer(proc_id)%my_real_array_1d(my_offset_5 + k),my_int_variable) ! get the remote proc id
              r_proc_id = my_integer
              r_address = 0

              do j=1,r_buffer_2_size(3,r_proc_id)
                my_integer = transfer(r_buffer_2(r_proc_id)%my_real_array_1d(r_address + 1),my_int_variable) ! get the id of the new segment
                segment_id_2 = abs(my_integer)
                my_integer = transfer(r_buffer_2(r_proc_id)%my_real_array_1d(r_address + 6),my_int_variable) ! get the edge of the new segment
                my_iedge_2 = my_integer
                my_integer = transfer(r_buffer_2(r_proc_id)%my_real_array_1d(r_address + 7),my_int_variable) ! get the number of r connected segment
                nb_r_connected_segment = my_integer

                ! check if the segment in the r_buffer_2 is the same new segment
                if((segment_id_2==segment_id).and.(my_iedge_2==my_iedge)) then


                  my_offset_1 = r_address + 7
                  my_offset_2 = r_address + 7+nb_r_connected_segment
                  my_offset_3 = r_address + 7+2*nb_r_connected_segment
                  my_offset_4 = r_address + 7+5*nb_r_connected_segment
                  my_offset_6 = r_address + 7+6*nb_r_connected_segment

                  do ijk=1,nb_r_connected_segment
                    my_integer = transfer(r_buffer_2(r_proc_id)%my_real_array_1d(my_offset_1 + ijk),my_int_variable) ! get global neighbour segment id
                    n_segment_id = abs(my_integer)
                    my_integer = transfer(r_buffer_2(r_proc_id)%my_real_array_1d(my_offset_2 + ijk),my_int_variable) ! get local neighbour segment id
                    local_n_segment_id = abs(my_integer)
                    my_integer = transfer(r_buffer_2(r_proc_id)%my_real_array_1d(my_offset_4 + ijk),my_int_variable) ! get edge id of the neighbour segment
                    n_iedge_id = my_integer
                    ierror = -1
                    call c_hash_find( segment_hash_id,n_segment_id,ierror ) ! check if "n_segment id" is already in the hash table
                    if(ierror==-1) then  ! no --> need to add it
                      seg_id = seg_id + 1
                      call c_hash_insert( segment_hash_id,n_segment_id,seg_id )
                      n_seg_id = seg_id
                      do kji=1,4
                        my_integer = transfer(r_buffer_2(r_proc_id)%my_real_array_1d(my_offset_6 + 4*(ijk-1) + kji),my_int_variable) ! check if the n_segment has already a neighbour
                        already_a_neighbour = my_integer
                        segment_pair(n_seg_id,kji,1) = already_a_neighbour
                      enddo
                    else
                      n_seg_id = ierror
                    endif

                    n_normal(1) = r_buffer_2(r_proc_id)%my_real_array_1d(my_offset_3 + 3*(ijk-1)+1)
                    n_normal(2) = r_buffer_2(r_proc_id)%my_real_array_1d(my_offset_3 + 3*(ijk-1)+2)
                    n_normal(3) = r_buffer_2(r_proc_id)%my_real_array_1d(my_offset_3 + 3*(ijk-1)+3)
                    my_criteria = EP30

                    ! -------
                    if(segment_id/=n_segment_id) then
                      call get_segment_criteria( my_criteria,normal,n_normal )
                      if(my_criteria<criteria(new_segment_id(i),my_iedge).and.segment_pair(n_seg_id,n_iedge_id,1)==0) then

                        criteria(new_segment_id(i),my_iedge) = my_criteria
                        criteria(n_seg_id,n_iedge_id) = my_criteria

                        previous_n_seg_id = segment_pair(new_segment_id(i),my_iedge,3) ! get the previous local segment id 
                        previous_iedge_id = segment_pair(new_segment_id(i),my_iedge,4) ! get the previous edge id

                        segment_pair(new_segment_id(i),my_iedge,1) = n_segment_id
                        segment_pair(new_segment_id(i),my_iedge,2) = r_proc_id
                        segment_pair(new_segment_id(i),my_iedge,3) = n_seg_id
                        segment_pair(new_segment_id(i),my_iedge,4) = n_iedge_id
                        segment_pair(new_segment_id(i),my_iedge,5) = local_n_segment_id

                        segment_pair(n_seg_id,n_iedge_id,1) = segment_id
                        segment_pair(n_seg_id,n_iedge_id,2) = proc_id
                        segment_pair(n_seg_id,n_iedge_id,3) = new_segment_id(i)
                        segment_pair(n_seg_id,n_iedge_id,4) = my_iedge
                        segment_pair(n_seg_id,n_iedge_id,5) = local_segment_id


                        if(previous_n_seg_id/=0) then
                          segment_pair(previous_n_seg_id,previous_iedge_id,1) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,2) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,3) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,4) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,5) = 0
                        endif
                      elseif(my_criteria==criteria(new_segment_id(i),my_iedge).and.segment_pair(n_seg_id,n_iedge_id,1)==0) then
                        if(n_segment_id<segment_pair(new_segment_id(i),my_iedge,1)) then
                          criteria(new_segment_id(i),my_iedge) = my_criteria
                          criteria(n_seg_id,n_iedge_id) = my_criteria

                          previous_n_seg_id = segment_pair(new_segment_id(i),my_iedge,3) ! get the previous local segment id 
                          previous_iedge_id = segment_pair(new_segment_id(i),my_iedge,4) ! get the previous edge id

                          segment_pair(new_segment_id(i),my_iedge,1) = n_segment_id
                          segment_pair(new_segment_id(i),my_iedge,2) = r_proc_id
                          segment_pair(new_segment_id(i),my_iedge,3) = n_seg_id
                          segment_pair(new_segment_id(i),my_iedge,4) = n_iedge_id
                          segment_pair(new_segment_id(i),my_iedge,5) = local_n_segment_id

                          segment_pair(n_seg_id,n_iedge_id,1) = segment_id
                          segment_pair(n_seg_id,n_iedge_id,2) = proc_id
                          segment_pair(n_seg_id,n_iedge_id,3) = new_segment_id(i)
                          segment_pair(n_seg_id,n_iedge_id,4) = my_iedge
                          segment_pair(n_seg_id,n_iedge_id,5) = local_segment_id

                        if(previous_n_seg_id/=0) then
                          segment_pair(previous_n_seg_id,previous_iedge_id,1) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,2) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,3) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,4) = 0
                          segment_pair(previous_n_seg_id,previous_iedge_id,5) = 0
                        endif
                        endif
                      endif
                    endif
                    ! -------
                  enddo
                endif
                r_address = r_address + 7+10*nb_r_connected_segment
              enddo
            enddo
            ! ---------------------
          enddo
          ! --------------------------

          ! --------------------------
          ! loop over the new active segment to update the neighbourhood
          do i=1,nb_new_segment
            proc_id = list_new_segment(permutation(i),1) ! get the processor of the new segment id
            segment_id = list_new_segment(permutation(i),3) ! get the global segment id
            local_segment_id = list_new_segment(permutation(i),6) ! get the local segment id
            my_iedge = list_new_segment(permutation(i),4) ! get the edge
            nin =  list_new_segment(permutation(i),5) ! get the interface id
            n_proc_id = segment_pair(new_segment_id(i),my_iedge,2) ! get the processor of the neighbour segment
            n_segment_id = segment_pair(new_segment_id(i),my_iedge,1) ! get the global neighbour segment id
            local_n_segment_id = segment_pair(new_segment_id(i),my_iedge,5) ! get the local neighbour segment id
            n_iedge_id = segment_pair(new_segment_id(i),my_iedge,4) ! get the edge of the neighbour segment
            if(local_n_segment_id/=0) then
              ! ---------------------
              ! the new active segment is only on proc_id --> update the neighbourhood
              if(ispmd==proc_id-1) then
                intbuf_tab(nin)%evoisin(4*(local_segment_id-1)+my_iedge) = n_iedge_id
                intbuf_tab(nin)%proc_mvoisin(4*(local_segment_id-1)+my_iedge) = n_proc_id
                if(n_proc_id==proc_id) then
                  my_id = local_n_segment_id
                else
                  my_id = -n_segment_id
                endif
                intbuf_tab(nin)%mvoisin(4*(local_segment_id-1)+my_iedge) = my_id
              endif
              ! ---------------------


              ! ---------------------
              ! the new active segment is only on n_proc_id --> update the neighbourhood
              if(ispmd==n_proc_id-1) then

                intbuf_tab(nin)%evoisin(4*(local_n_segment_id-1)+n_iedge_id) = my_iedge
                intbuf_tab(nin)%proc_mvoisin(4*(local_n_segment_id-1)+n_iedge_id) = proc_id
                if(n_proc_id==proc_id) then
                  my_id = local_segment_id
                else
                  my_id = -segment_id
                endif

                intbuf_tab(nin)%mvoisin(4*(local_n_segment_id-1)+n_iedge_id) = my_id
              endif
              ! ---------------------
            endif
          enddo
          ! --------------------------

          deallocate( list_new_segment )
          deallocate( new_segment_id )
          deallocate( permutation )

          call c_delete_hash( segment_hash_id )

!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine update_neighbour_segment
      end module update_neighbour_segment_mod
