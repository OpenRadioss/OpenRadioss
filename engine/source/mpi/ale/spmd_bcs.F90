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
!||    spmd_bcs_mod   ../engine/source/mpi/ale/spmd_bcs.F90
!||--- called by ------------------------------------------------------
!||    resol          ../engine/source/engine/resol.F
!||====================================================================
      module spmd_bcs_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief 
!! \details
!||====================================================================
!||    spmd_bcs                   ../engine/source/mpi/ale/spmd_bcs.F90
!||--- called by ------------------------------------------------------
!||    resol                      ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    alloc_my_real_2d_array     ../common_source/modules/array_mod.F
!||    dealloc_my_real_2d_array   ../common_source/modules/array_mod.F
!||--- uses       -----------------------------------------------------
!||    array_mod                  ../common_source/modules/array_mod.F
!||    bcs_mod                    ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    nodal_arrays_mod           ../common_source/modules/nodal_arrays.F90
!||    spmd_mod                   ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine spmd_bcs(ispmd,nspmd,iparit,bcs,nodes)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use array_mod
          use spmd_mod
          use nodal_arrays_mod , only : nodal_arrays_
          use bcs_mod , only : bcs_struct_
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
          integer, intent(in) :: ispmd !< processor id
          integer, intent(in) :: nspmd !< number of processors
          integer, intent(in) :: iparit !< parallelization type (0: parith/off, 1: parith/on)
          type(bcs_struct_), intent(inout) :: bcs !< boundary conditions
          type(nodal_arrays_), intent(inout) :: nodes !< nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,ijk
          integer :: my_size
          integer :: node_id
          integer :: my_address
          integer :: proc_id
          integer :: next,low_bound,up_bound
          integer :: r_proc_nb,s_proc_nb
          integer, dimension(nspmd) :: r_index,r_req,s_req
          type(array_type_my_real_2d), dimension(:), allocatable :: s_buffer,r_buffer
          integer, parameter :: msgoff = 3006
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          r_proc_nb = 0
          s_proc_nb = 0
          allocate(s_buffer(nspmd))
          allocate(r_buffer(nspmd))          

          if(iparit==0) then
            ! -------------
            do i=1,nspmd
              my_size = nodes%boundary_add(1,i+1) - nodes%boundary_add(1,i)
              if(ispmd/=i-1.and.my_size>0) then
                r_proc_nb = r_proc_nb + 1
                r_buffer(i)%size_my_real_array_2d(1) = 3
                r_buffer(i)%size_my_real_array_2d(2) = my_size
                r_index(r_proc_nb) = i
                call alloc_my_real_2d_array(r_buffer(i))
                call spmd_irecv(r_buffer(i)%my_real_array_2d(1,1),3*my_size,i-1,msgoff,r_req(r_proc_nb))
              endif
            enddo
            ! -------------

            ! -------------
            do i=1,nspmd
              my_size = nodes%boundary_add(1,i+1) - nodes%boundary_add(1,i)
              if(ispmd/=i-1.and.my_size>0) then
                next = 0
                s_buffer(i)%size_my_real_array_2d(1) = 3
                s_buffer(i)%size_my_real_array_2d(2) = my_size
                call alloc_my_real_2d_array(s_buffer(i))
                do j=nodes%boundary_add(1,i),nodes%boundary_add(1,i+1)-1
                  node_id = nodes%boundary(j)
                  next = next + 1
                  s_buffer(i)%my_real_array_2d(1:3,next) = bcs%la_nrf(1:3,node_id)
                enddo
                s_proc_nb = s_proc_nb + 1
                call spmd_isend(s_buffer(i)%my_real_array_2d(1,1),3*my_size,i-1,msgoff,s_req(s_proc_nb))
              endif
            enddo
            ! -------------          

            ! -------------
            do i=1,r_proc_nb
              call spmd_waitany(r_req,r_proc_nb,ijk)
              proc_id = r_index(ijk) ! get the R processor id
              do j=nodes%boundary_add(1,proc_id),nodes%boundary_add(1,proc_id+1)-1
                node_id = nodes%boundary(j)
                my_address = j - nodes%boundary_add(1,proc_id) + 1
                bcs%la_nrf(1:3,node_id) = bcs%la_nrf(1:3,node_id) + r_buffer(proc_id)%my_real_array_2d(1:3,my_address)
              enddo
              call dealloc_my_real_2d_array(r_buffer(proc_id))
            enddo
            ! -------------

            ! -------------
            call spmd_waitall(s_req,s_proc_nb)
            do i=1,nspmd
              my_size = nodes%boundary_add(1,i+1) - nodes%boundary_add(1,i)
              if(ispmd/=i-1.and.my_size>0) then
                call dealloc_my_real_2d_array(s_buffer(i))
              endif
            enddo
            ! -------------
          else
            ! -------------
            do i=1,nspmd
              my_size = bcs%cfl_nrf%ddm(i)%r_cont_nb
              if(ispmd/=i-1.and.my_size>0) then
                r_proc_nb = r_proc_nb + 1
                r_buffer(i)%size_my_real_array_2d(1) = 3
                r_buffer(i)%size_my_real_array_2d(2) = my_size
                r_index(r_proc_nb) = i
                call alloc_my_real_2d_array(r_buffer(i))
                call spmd_irecv(r_buffer(i)%my_real_array_2d(1,1),3*my_size,i-1,msgoff,r_req(r_proc_nb))
              endif
            enddo
            ! -------------

            ! -------------
            do i=1,nspmd
              my_size = bcs%cfl_nrf%ddm(i)%s_cont_nb
              if(ispmd/=i-1.and.my_size>0) then
                s_buffer(i)%size_my_real_array_2d(1) = 3
                s_buffer(i)%size_my_real_array_2d(2) = my_size
                call alloc_my_real_2d_array(s_buffer(i))
                do j=1,bcs%cfl_nrf%ddm(i)%s_cont_nb
                  my_address = bcs%cfl_nrf%ddm(i)%send_iadfsky(j) ! get the address of the local contribution for this node on the sending domain
                  s_buffer(i)%my_real_array_2d(1:3,j) = bcs%cfl_nrf%fsky(1:3,my_address)              
                enddo
                s_proc_nb = s_proc_nb + 1
                call spmd_isend(s_buffer(i)%my_real_array_2d(1,1),3*my_size,i-1,msgoff,s_req(s_proc_nb))
              endif
            enddo
            ! -------------          

            ! -------------
            do i=1,r_proc_nb
              call spmd_waitany(r_req,r_proc_nb,ijk)
              proc_id = r_index(ijk) ! get the R processor id
              do j=1,bcs%cfl_nrf%ddm(proc_id)%r_cont_nb
                my_address = bcs%cfl_nrf%ddm(proc_id)%rcv_iadfsky(j) ! get the address of the local contribution for this node on the receiving domain                 
                bcs%cfl_nrf%fsky(1:3,my_address) = r_buffer(proc_id)%my_real_array_2d(1:3,j)
              enddo
              call dealloc_my_real_2d_array(r_buffer(proc_id))
            enddo
            ! -------------

            ! -------------
            call spmd_waitall(s_req,s_proc_nb)
            do i=1,nspmd
              my_size = bcs%cfl_nrf%ddm(i)%s_cont_nb
              if(ispmd/=i-1.and.my_size>0) then
                call dealloc_my_real_2d_array(s_buffer(i))
              endif
            enddo
            ! -------------          

            do i = 1, bcs%nrf_num_nodes
              node_id = bcs%nrf_node_ids(i)
              low_bound = bcs%cfl_nrf%nod_iadsky(i)
              up_bound = bcs%cfl_nrf%nod_iadsky(i+1) - 1
              do j=low_bound,up_bound              
                bcs%la_nrf(1:3,node_id) = bcs%la_nrf(1:3,node_id) + bcs%cfl_nrf%fsky(1:3,j)           
              end do
            end do          
          endif

          deallocate(s_buffer)
          deallocate(r_buffer)          
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine spmd_bcs
      end module spmd_bcs_mod
