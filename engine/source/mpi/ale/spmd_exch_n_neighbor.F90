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
!||    spmd_exch_n_neighbor_mod        ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||====================================================================
      module spmd_exch_n_neighbor_mod
        implicit none

        ! \brief Interface for spmd_irecv, a wrapper for MPI_IRECV
        interface spmd_exch_n_neighbor
          module procedure spmd_exch_n_neighbor_2d !< 2d version
          module procedure spmd_exch_n_neighbor_3d !< 3d version
        end interface spmd_exch_n_neighbor        
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief
!! \details
!||====================================================================
!||    spmd_exch_n_neighbor            ../engine/source/mpi/ale/spmd_exch_n_neighbor.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||--- calls      -----------------------------------------------------
!||    alloc_my_real_2d_array          ../common_source/modules/array_mod.F
!||    dealloc_my_real_2d_array        ../common_source/modules/array_mod.F
!||    spmd_waitall                    ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany                    ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    array_mod                       ../common_source/modules/array_mod.F
!||    debug_mod                       ../engine/share/modules/debug_mod.F
!||    precision_mod                   ../common_source/modules/precision_mod.F90
!||    spmd_mod                        ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine spmd_exch_n_neighbor_2d(flag,nspmd,n_entity,dim1,s_lesdvois,s_lercvois, &
                                        s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, &
                                        nesdvois,nercvois,lesdvois,lercvois, &
                                        phi,s_buffer,r_buffer)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use array_mod
          use spmd_mod



          use debug_mod
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
          integer, intent(in) :: flag !< flag for the mpi comms : (0) --> irecv + isend, (1)--> wait + update
          integer, intent(in) :: nspmd !< number of processors
          integer, intent(in) :: n_entity !< number of entity (= numels/q + sum of the numbers of frontier elements (rcv))
          integer, intent(in) :: dim1 !< second dimension of phi array
          integer, intent(in) :: s_lesdvois !< size of lesdvois array
          integer, intent(in) :: s_lercvois !< size of lercvois array       
          integer, intent(inout) :: s_proc_nb !< number of S procs
          integer, intent(inout) :: r_proc_nb !< number of R procs
          integer, dimension(nspmd), intent(inout) :: s_index !< index of S processor
          integer, dimension(nspmd), intent(inout) :: r_index !< index of R processor
          integer, dimension(nspmd), intent(inout) :: s_req !< S requests
          integer, dimension(nspmd), intent(inout) :: r_req !< R requests
          integer, dimension(nspmd+1), intent(in) :: nesdvois !< number of frontier elements (send)          
          integer, dimension(nspmd+1), intent(in) :: nercvois !< number of frontier elements (rcv)
          integer, dimension(s_lesdvois), intent(in) :: lesdvois !< frontier element ids (send)
          integer, dimension(s_lercvois), intent(in) :: lercvois !< frontier element ids (rcv)
          real(kind=WP), dimension(n_entity,dim1), intent(inout) :: phi !<values to exchange
          type(array_type_my_real_2d), dimension(nspmd), intent(inout) :: s_buffer !< send buffer
          type(array_type_my_real_2d), dimension(nspmd), intent(inout) :: r_buffer !< send buffer     
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: p,j,k,ijk
          integer :: s_address,my_address
          integer, dimension(nspmd+1) :: r_address
          integer :: s_size,r_size
          integer :: proc_id
          integer :: msgtyp
          integer, parameter :: msgoff = 3005
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if(flag==0) then
            msgtyp = msgoff
            ! -------------
            ! post the reception
            r_proc_nb = 0
            do p=1,nspmd
              r_size = nercvois(p)
              if (r_size > 0) then
                r_proc_nb = r_proc_nb + 1
                r_index(r_proc_nb) = p
                r_buffer(p)%size_my_real_array_2d(1) = r_size
                r_buffer(p)%size_my_real_array_2d(2) = dim1
                call alloc_my_real_2d_array(r_buffer(p))
                call spmd_irecv(r_buffer(p)%my_real_array_2d(1,1),dim1*r_size,p-1,msgtyp,r_req(r_proc_nb))
              end if
            end do
            ! -------------

            ! -------------
            ! initialize the send buffer & send it
            s_address = 0
            s_proc_nb = 0
            do p=1,nspmd
              s_size = nesdvois(p)
              if (s_size > 0) then
                s_buffer(p)%size_my_real_array_2d(1) = s_size
                s_buffer(p)%size_my_real_array_2d(2) = dim1
                call alloc_my_real_2d_array(s_buffer(p))
                do k=1,dim1
                  do j=1,s_size
                    ijk = lesdvois(s_address+j)                
                    s_buffer(p)%my_real_array_2d(j,k) = phi(ijk,k)
                  end do
                enddo
                s_address = s_address + s_size
                ! send the data
                s_proc_nb = s_proc_nb + 1
                s_index(s_proc_nb) = p   
                call spmd_isend(s_buffer(p)%my_real_array_2d(1,1),dim1*s_size,p-1,msgtyp,s_req(s_proc_nb))
              end if
            end do
          else
            r_address(1) = 0
            do p=2,nspmd+1
              r_size = nercvois(p-1)
              r_address(p) = r_address(p-1) + r_size
            end do

            ! -------------
            ! wait for the receive comms and update values + deallocation
            do p=1,r_proc_nb
              call spmd_waitany(r_proc_nb,r_req,ijk)
              proc_id = r_index(ijk) ! get the R processor id
              r_size = nercvois(proc_id)
              do k=1,dim1
                do j=1,r_size
                  my_address = lercvois(r_address(proc_id)+j)
                  phi(my_address,k) = r_buffer(proc_id)%my_real_array_2d(j,k)
                enddo
              enddo
              call dealloc_my_real_2d_array(r_buffer(proc_id))
            end do
            ! -------------

            ! -------------
            ! wait for the send comms + deallocation
            call spmd_waitall(s_proc_nb,s_req)
            do p=1,nspmd
              s_size = nesdvois(p)
              if (s_size > 0) then
                call dealloc_my_real_2d_array(s_buffer(p))
              end if
            end do            
            ! -------------
          endif
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine spmd_exch_n_neighbor_2d
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief
!! \details
        subroutine spmd_exch_n_neighbor_3d(flag,nspmd,n_entity,dim1,dim2,s_lesdvois,s_lercvois, &
                                        s_proc_nb,r_proc_nb,s_index,r_index,s_req,r_req, &
                                        nesdvois,nercvois,lesdvois,lercvois, &
                                        phi,s_buffer,r_buffer)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use array_mod
          use spmd_mod



          use debug_mod
          use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: flag !< flag for the mpi comms : (0) --> irecv + isend, (1)--> wait + update
          integer, intent(in) :: nspmd !< number of processors
          integer, intent(in) :: n_entity !< number of entity (= numels/q + sum of the numbers of frontier elements (rcv))
          integer, intent(in) :: dim1 !< second dimension of phi array
          integer, intent(in) :: dim2 !< third dimension of phi array
          integer, intent(in) :: s_lesdvois !< size of lesdvois array
          integer, intent(in) :: s_lercvois !< size of lercvois array       
          integer, intent(inout) :: s_proc_nb !< number of S procs
          integer, intent(inout) :: r_proc_nb !< number of R procs
          integer, dimension(nspmd), intent(inout) :: s_index !< index of S processor
          integer, dimension(nspmd), intent(inout) :: r_index !< index of R processor
          integer, dimension(nspmd), intent(inout) :: s_req !< S requests
          integer, dimension(nspmd), intent(inout) :: r_req !< R requests
          integer, dimension(nspmd+1), intent(in) :: nesdvois !< number of frontier elements (send)          
          integer, dimension(nspmd+1), intent(in) :: nercvois !< number of frontier elements (rcv)
          integer, dimension(s_lesdvois), intent(in) :: lesdvois !< frontier element ids (send)
          integer, dimension(s_lercvois), intent(in) :: lercvois !< frontier element ids (rcv)
          real(kind=WP), dimension(n_entity,dim1,dim2), intent(inout) :: phi !<values to exchange
          type(array_type_my_real_3d), dimension(nspmd), intent(inout) :: s_buffer !< send buffer
          type(array_type_my_real_3d), dimension(nspmd), intent(inout) :: r_buffer !< send buffer     
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: p,j,k,ijk,l
          integer :: s_address,my_address
          integer, dimension(nspmd+1) :: r_address
          integer :: s_size,r_size
          integer :: proc_id
          integer :: msgtyp,IERROR
          integer, parameter :: msgoff = 3006
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if(flag==0) then
            msgtyp = msgoff
            ! -------------
            ! post the reception
            r_proc_nb = 0
            do p=1,nspmd
              r_size = nercvois(p)
              if (r_size > 0) then
                r_proc_nb = r_proc_nb + 1
                r_index(r_proc_nb) = p
                r_buffer(p)%size_my_real_array_3d(1) = r_size
                r_buffer(p)%size_my_real_array_3d(2) = dim1
                r_buffer(p)%size_my_real_array_3d(3) = dim2
                call alloc_my_real_3d_array(r_buffer(p))
                call spmd_irecv(r_buffer(p)%my_real_array_3d(1,1,1),dim2*dim1*r_size,p-1,msgtyp,r_req(r_proc_nb))
                !call MPI_IRECV(r_buffer(p)%my_real_array_3d, dim2*dim1*r_size, MPI_DOUBLE_PRECISION, p-1,&
                !    msgtyp, MPI_COMM_WORLD, r_req(r_proc_nb),IERROR)
              end if
            end do
            ! -------------

            ! -------------
            ! initialize the send buffer & send it
            s_address = 0
            s_proc_nb = 0
            do p=1,nspmd
              s_size = nesdvois(p)
              if (s_size > 0) then
                s_buffer(p)%size_my_real_array_3d(1) = s_size
                s_buffer(p)%size_my_real_array_3d(2) = dim1
                s_buffer(p)%size_my_real_array_3d(3) = dim2
                call alloc_my_real_3d_array(s_buffer(p))
                do l=1,dim2
                  do k=1,dim1
                    do j=1,s_size
                      ijk = lesdvois(s_address+j)                
                      s_buffer(p)%my_real_array_3d(j,k,l) = phi(ijk,k,l)
                    end do
                  enddo
                enddo
                s_address = s_address + s_size
                ! send the data
                s_proc_nb = s_proc_nb + 1
                s_index(s_proc_nb) = p   
                call spmd_isend(s_buffer(p)%my_real_array_3d(1,1,1),dim2*dim1*s_size,p-1,msgtyp,s_req(s_proc_nb))  
                !call mpi_isend(s_buffer(p)%my_real_array_3d,dim2*dim1*s_size,MPI_DOUBLE_PRECISION, &
                !p-1,msgtyp,MPI_COMM_WORLD,s_req(s_proc_nb),ierror)                  
              end if
            end do
          else
            r_address(1) = 0
            do p=2,nspmd+1
              r_size = nercvois(p-1)
              r_address(p) = r_address(p-1) + r_size
            end do

            ! -------------
            ! wait for the receive comms and update values + deallocation
            do p=1,r_proc_nb            
              call spmd_waitany(r_proc_nb,r_req,ijk)
              proc_id = r_index(ijk) ! get the R processor id
              r_size = nercvois(proc_id)
              do l=1,dim2
                do k=1,dim1
                  do j=1,r_size
                    my_address = lercvois(r_address(proc_id)+j)
                    phi(my_address,k,l) = r_buffer(proc_id)%my_real_array_3d(j,k,l)
                  enddo
                enddo
              enddo
              call dealloc_my_real_3d_array(r_buffer(proc_id))
            end do
            ! -------------

            ! -------------
            ! wait for the send comms + deallocation
            call spmd_waitall(s_proc_nb,s_req)
            do p=1,nspmd
              s_size = nesdvois(p)
              if (s_size > 0) then
                call dealloc_my_real_3d_array(s_buffer(p))
              end if
            end do            
            ! -------------
          endif
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine spmd_exch_n_neighbor_3d        
      end module spmd_exch_n_neighbor_mod
