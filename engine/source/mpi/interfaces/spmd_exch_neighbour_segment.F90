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
!||    spmd_exch_neighbour_segment_mod   ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    get_neighbour_surface             ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||====================================================================
      module spmd_exch_neighbour_segment_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief This routine exchanges data between processors for the solid erosion / interface type 25 algorithm
!! \details the routine is divided in 2 parts :
!!          * part 1 : + exchange between local --> remote proc ( s_buffer / r_buffer )
!!                     + remote proc checks if some remote segments are connected to the new active segment
!!                     + remote proc saves the list of remote segments in s_buffer_2
!!          * part 2 : exchange between remote proc --> local proc + other remote procs ( s_buffer_2 / r_buffer_2)
!!         for part 1 & 2 : 2 mpi comms
!!          * comm 1 : exchange of mpi buffer size 
!!          * comm 2 : exchange of mpi buffer
!!
!!         Example : 
!!          [12] sends to [2] / [4] / [100] a new active segment A (s_buffer / r_buffer)
!!          [2] / [4] / [100] check if there have locally a segment connected to A (data are saved in s_buffer_2)
!!          [2] / [4] / [100] sends theirs buffers to the local proc + other remote procs (s_buffer_2 / r_buffer_2)
!!          [2] --> [12] / [4] / [100]
!!          [4] --> [12] / [2] / [100]
!!          [100] --> [12] / [2] / [4]
!!
!!

!||====================================================================
!||    spmd_exch_neighbour_segment                  ../engine/source/mpi/interfaces/spmd_exch_neighbour_segment.F90
!||--- called by ------------------------------------------------------
!||    get_neighbour_surface                        ../engine/source/interfaces/interf/get_neighbour_surface.F90
!||--- calls      -----------------------------------------------------
!||    alloc_my_real_1d_array                       ../common_source/modules/array_mod.F
!||    get_neighbour_surface_from_remote_proc       ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||    spmd_wait                                    ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany                                 ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    array_mod                                    ../common_source/modules/array_mod.F
!||    get_neighbour_surface_from_remote_proc_mod   ../engine/source/interfaces/interf/get_neighbour_surface_from_remote_proc.F90
!||    intbufdef_mod                                ../common_source/modules/interfaces/intbufdef_mod.F90
!||    nodal_arrays_mod                             ../common_source/modules/nodal_arrays.F90
!||    precision_mod                                ../common_source/modules/precision_mod.F90
!||    shooting_node_mod                            ../engine/share/modules/shooting_node_mod.F
!||    spmd_mod                                     ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine spmd_exch_neighbour_segment(nspmd,ispmd, &
                                                ninter,numnod, &
                                                s_buffer_size,r_buffer_size,s_buffer_2_size,r_buffer_2_size,&
                                                iad_elem,nodes,x, &
                                                s_buffer,r_buffer,s_buffer_2,r_buffer_2, &
                                                intbuf_tab,shoot_struct)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use spmd_mod
          use intbufdef_mod , only : intbuf_struct_
          use shooting_node_mod , only : shooting_node_type
          use get_neighbour_surface_from_remote_proc_mod , only : get_neighbour_surface_from_remote_proc
          use array_mod , only : array_type,alloc_my_real_1d_array,dealloc_my_real_1d_array
          use nodal_arrays_mod, only : get_local_node_id, nodal_arrays_
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nspmd !< number of processor
          integer, intent(in) :: ispmd !< processor id
          integer, intent(in) :: ninter !< number of interface
          integer, intent(in) :: numnod !< number of node
          integer, dimension(2,nspmd), intent(inout) :: s_buffer_size !< size of S buffer
          integer, dimension(2,nspmd), intent(inout) :: r_buffer_size !< size of R buffer
          integer, dimension(3,nspmd), intent(inout) :: s_buffer_2_size !< size of S buffer
          integer, dimension(3,nspmd), intent(inout) :: r_buffer_2_size !< size of R buffer
          integer, dimension(2,nspmd+1), intent(in) :: iad_elem !< frontier between processor
          type(nodal_arrays_), intent(in) :: nodes !< nodal arrays
          real(kind=WP), dimension(3,numnod), intent(in) :: x !< nodal position
          type(array_type), dimension(nspmd), intent(inout) :: s_buffer !< mpi buffer (send)
          type(array_type), dimension(nspmd), intent(inout) :: r_buffer !< mpi buffer (rcv)
          type(array_type), dimension(nspmd), intent(inout) :: s_buffer_2 !< mpi buffer (send)
          type(array_type), dimension(nspmd), intent(inout) :: r_buffer_2 !< mpi buffer (rcv)
          type(intbuf_struct_), dimension(ninter), intent(inout) :: intbuf_tab    !< interface data 
          type(shooting_node_type), intent(inout) :: shoot_struct !< structure for shooting node algo

! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
          integer :: recv_nb_1,recv_nb_2,recv_nb_3,recv_nb_4
          integer :: my_size,my_index
          integer :: frontier_elm
          integer, parameter :: spmd_tag_1 = 13016
          integer, parameter :: spmd_tag_2 = 13017
          integer, parameter :: spmd_tag_3 = 13018
          integer, parameter :: spmd_tag_4 = 13019

          integer :: proc_id
          integer, dimension(nspmd) :: request_s_1,request_r_1
          integer, dimension(nspmd) :: request_s_2,request_r_2
          integer, dimension(nspmd) :: request_s_3,request_r_3
          integer, dimension(nspmd) :: request_s_4,request_r_4
          integer, dimension(nspmd) :: index_r_proc,index_r_proc_2,index_r_proc_3,index_r_proc_4
          integer :: status_mpi(mpi_status_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   external functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   body
! ----------------------------------------------------------------------------------------------------------------------
!
          ! --------------------------
          ! ----------------
          ! receive the data : "size if my R buffer"
          recv_nb_1 = 0        
          do i=1,nspmd
              frontier_elm = iad_elem(1,i+1)-iad_elem(1,i) ! check if the proc "i" is a neighbour
              if(frontier_elm>0) then
                recv_nb_1 = recv_nb_1 + 1
                index_r_proc(recv_nb_1) = i                    
                call spmd_irecv(r_buffer_size(1:2,i),2,i-1,spmd_tag_1,request_r_1(recv_nb_1),SPMD_COMM_WORLD)
              elseif(ispmd==i-1) then
                r_buffer_size(1:2,i) = s_buffer_size(1:2,i)
                r_buffer(i)%size_my_real_array_1d = r_buffer_size(1,i)
                call alloc_my_real_1d_array(r_buffer(i))
                r_buffer(i)%my_real_array_1d(1:r_buffer_size(1,i)) = s_buffer(i)%my_real_array_1d(1:s_buffer_size(1,i))
              endif
          enddo
          ! ----------------
          ! ----------------
          ! send the data : "size if my S buffer"
          do i=1,nspmd
              frontier_elm = iad_elem(1,i+1)-iad_elem(1,i) ! check if the proc "i" is a neighbour
              if(frontier_elm>0) then
                  call spmd_isend(s_buffer_size(1:2,i),2,i-1,spmd_tag_1,request_s_1(i),SPMD_COMM_WORLD)
              endif
          enddo
          ! ----------------

          ! ----------------
          ! wait the r comm "size if my R buffer"
          ! --> to allocate the r buffer
          ! --> and post the rcv comm "list of potential remote segment" 
          recv_nb_2 = 0
          if(recv_nb_1>0) then
            do i=1,recv_nb_1
                call spmd_waitany(recv_nb_1, request_r_1, my_index, status_mpi)
                proc_id = index_r_proc(my_index)

                r_buffer(proc_id)%size_my_real_array_1d = r_buffer_size(1,proc_id)
                call alloc_my_real_1d_array(r_buffer(proc_id))
                if(r_buffer_size(1,proc_id)>0) then
                  recv_nb_2 = recv_nb_2 + 1
                  index_r_proc_2(recv_nb_2) = proc_id
                  my_size = r_buffer(proc_id)%size_my_real_array_1d
                  call spmd_irecv( r_buffer(proc_id)%my_real_array_1d,my_size,proc_id-1,              &
                                   spmd_tag_2,request_r_2(recv_nb_2),SPMD_COMM_WORLD )
                endif
            enddo
          endif
          ! ----------------

          ! ----------------
          ! send the data : "list of potential remote segment" 
          do i=1,nspmd
              frontier_elm = iad_elem(1,i+1)-iad_elem(1,i) ! check if the proc "i" is a neighbour
              if(s_buffer_size(1,i)>0) then
                  my_size = s_buffer_size(1,i)
                  call spmd_isend(s_buffer(i)%my_real_array_1d,my_size,i-1,spmd_tag_2,request_s_2(i),SPMD_COMM_WORLD)
              endif
          enddo
          ! ----------------

          ! ----------------
          ! wait the r comm "list of potential remote segment" and :
          !   * check if a segment is connected to the 2 s nodes
          !   * send the result to S proc "list of connected segment"
          do i=1,nspmd
            s_buffer_2_size(1:3,i) = 0
          enddo
          if(recv_nb_2>0) then
            do i=1,nspmd
              s_buffer_2(i)%size_my_real_array_1d = 8 + 13 * shoot_struct%max_surf_nb + 3 +3
              call alloc_my_real_1d_array(s_buffer_2(i))
            enddo

            do i=1,recv_nb_2
                call spmd_waitany(recv_nb_2, request_r_2, my_index, status_mpi)
                proc_id = index_r_proc_2(my_index)
                call get_neighbour_surface_from_remote_proc( ninter,numnod,nspmd,  &
                                                             r_buffer_size(1,proc_id),r_buffer_size(2,proc_id),s_buffer_2_size, &
                                                             nodes,r_buffer(proc_id)%my_real_array_1d,s_buffer_2, &
                                                             x,intbuf_tab,shoot_struct ,&
                                                             ispmd,proc_id )
            enddo
          endif

          do i=1,nspmd
            frontier_elm = iad_elem(1,i+1)-iad_elem(1,i) ! check if the proc "i" is a neighbour
            if(frontier_elm>0) then
              call spmd_isend(s_buffer_2_size(:,i),3,i-1,spmd_tag_3,request_s_3(i),SPMD_COMM_WORLD)
              if(s_buffer_2_size(1,i)>0) then
                call spmd_isend(s_buffer_2(i)%my_real_array_1d,s_buffer_2_size(1,i),i-1,  &
                                  spmd_tag_4,request_s_4(i) )
              endif
            endif
          enddo
          ! ----------------

          ! ----------------
          ! wait the 2 s comm : "size if my S buffer" & "list of potential remote segment" 
          ! and post the R comm : "size of my R_2 buffer"
          recv_nb_3 = 0
          do i=1,nspmd
              frontier_elm = iad_elem(1,i+1)-iad_elem(1,i) ! check if the proc "i" is a neighbour
              if(frontier_elm>0) then
                call spmd_wait(request_s_1(i), status_mpi)
                if(s_buffer_size(1,i)>0) call spmd_wait(request_s_2(i), status_mpi)
                recv_nb_3 = recv_nb_3 + 1
                index_r_proc_3(recv_nb_3) = i
                call spmd_irecv(r_buffer_2_size(:,i),3,i-1,spmd_tag_3,request_r_3(recv_nb_3), SPMD_COMM_WORLD)

              elseif(ispmd==i-1) then
                r_buffer_2_size(1:3,i) =s_buffer_2_size(1:3,i)
                r_buffer_2(i)%size_my_real_array_1d = r_buffer_2_size(1,i)
                call alloc_my_real_1d_array(r_buffer_2(i))
                r_buffer_2(i)%my_real_array_1d(1:r_buffer_2_size(1,i)) = s_buffer_2(i)%my_real_array_1d(1:s_buffer_2_size(1,i))
              endif
          enddo

          ! wait the R comm "data of remote proc"
          recv_nb_4 = 0
          if(recv_nb_3>0) then
            do i=1,recv_nb_3
              call spmd_waitany(recv_nb_3, request_r_3, my_index, status_mpi)
              proc_id = index_r_proc_3(my_index)
              r_buffer_2(proc_id)%size_my_real_array_1d = r_buffer_2_size(1,proc_id)

              call alloc_my_real_1d_array(r_buffer_2(proc_id))
              if(r_buffer_2_size(1,proc_id)>0) then
                recv_nb_4 = recv_nb_4 + 1
                index_r_proc_4(recv_nb_4) = proc_id
                my_size = r_buffer_2_size(1,proc_id)
                call spmd_irecv( r_buffer_2(proc_id)%my_real_array_1d,my_size, &
                                 proc_id-1,spmd_tag_4,request_r_4(recv_nb_4), SPMD_COMM_WORLD )
              endif
            enddo
          endif
          ! ----------------

          ! ----------------
          ! wait the R comm "list of connected segment"
          do i=1,recv_nb_4
            call spmd_waitany(recv_nb_4, request_r_4, my_index, status_mpi)
          enddo
          ! ----------------

          ! ----------------
          ! wait the S comm "size of my S_2 buffer" & "list of connected segment"
          do i=1,nspmd
            frontier_elm = iad_elem(1,i+1)-iad_elem(1,i) ! check if the proc "i" is a neighbour
            if(frontier_elm>0) then
              call spmd_wait(request_s_3(i),status_mpi)
              if(s_buffer_2_size(1,i)>0) then
                call spmd_wait(request_s_4(i),status_mpi)
              endif
            endif
          enddo
          ! ----------------

          ! --------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine spmd_exch_neighbour_segment
      end module spmd_exch_neighbour_segment_mod
