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
!||    spmd_exch_min_max_mod            ../engine/source/mpi/ale/spmd_exch_min_max.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
!||====================================================================
      module spmd_exch_min_max_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief 
!! \details
!||====================================================================
!||    spmd_exch_min_max                ../engine/source/mpi/ale/spmd_exch_min_max.F90
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
!||--- calls      -----------------------------------------------------
!||    spmd_waitall                     ../engine/source/mpi/spmd_wait.F90
!||    spmd_waitany                     ../engine/source/mpi/spmd_wait.F90
!||--- uses       -----------------------------------------------------
!||    debug_mod                        ../engine/share/modules/debug_mod.F
!||    precision_mod                    ../common_source/modules/precision_mod.F90
!||    spmd_mod                         ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        subroutine spmd_exch_min_max(flag,nspmd,numnod,lencom,s_proc_nb,r_proc_nb,            & 
                                     s_fr_elem,s_index,r_index,s_req,r_req,fr_elem,iad_elem,            &
                                     min_value,max_value,s_buffer,r_buffer)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use spmd_mod






          USE DEBUG_MOD
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
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: lencom !< length of communication buffer
          integer, intent(inout) :: s_proc_nb !< number of S procs
          integer, intent(inout) :: r_proc_nb !< number of R procs
          integer, intent(in) :: s_fr_elem !< size of fr_elem array
          integer, dimension(nspmd), intent(inout) :: s_index !< index of S processor
          integer, dimension(nspmd), intent(inout) :: r_index !< index of R processor
          integer, dimension(nspmd), intent(inout) :: s_req !< S requests
          integer, dimension(nspmd), intent(inout) :: r_req !< R requests
          integer, dimension(s_fr_elem), intent(in) :: fr_elem !< frontier elements & nodes
          integer, dimension(2,nspmd+1), intent(in) :: iad_elem !< number of frontier elements & nodes
          real(kind=WP), dimension(numnod,3), intent(inout) :: min_value !< minimum values to exchange
          real(kind=WP), dimension(numnod,3), intent(inout) :: max_value !< maximum values to exchange
          real(kind=WP), dimension(2*lencom*3), intent(inout) :: s_buffer !< send buffer
          real(kind=WP), dimension(2*lencom*3), intent(inout) :: r_buffer !< receive buffer        
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: p,j,ijk
          integer :: s_address,s_address_2,r_address
          integer :: s_size,r_size
          integer :: node_id,proc_id
          integer :: msgtyp
          integer, parameter :: msgoff = 400       
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
            r_address = 1
            r_proc_nb = 0
            do p=1,nspmd
              r_size = iad_elem(1,p+1) - iad_elem(1,p)
              if (r_size > 0) then
                r_proc_nb = r_proc_nb + 1
                r_index(r_proc_nb) = p
                call spmd_irecv(r_buffer(r_address),2*r_size*3,p-1,msgtyp,r_req(r_proc_nb))
              end if
              r_address = r_address + 2*r_size*3
            end do
            ! -------------

            ! -------------
            ! initialize the send buffer & send it
            s_address = 1
            s_address_2 = 1
            s_proc_nb = 0
            do p=1,nspmd
              s_size = iad_elem(1,p+1) - iad_elem(1,p)
              if (s_size > 0) then
                do j=iad_elem(1,p),iad_elem(1,p+1)-1
                  node_id = fr_elem(j)
                  s_buffer(s_address_2) = min_value(node_id,1)
                  s_buffer(s_address_2+1) = max_value(node_id,1)
                  s_buffer(s_address_2+2) = min_value(node_id,2)
                  s_buffer(s_address_2+3) = max_value(node_id,2)
                  s_buffer(s_address_2+4) = min_value(node_id,3)
                  s_buffer(s_address_2+5) = max_value(node_id,3)
                  s_address_2 = s_address_2 + 6
                end do
                ! send the data
                s_proc_nb = s_proc_nb + 1
                s_index(s_proc_nb) = p                
                call spmd_isend(s_buffer(s_address),2*s_size*3,p-1,msgtyp,s_req(s_proc_nb))
                s_address = s_address + 2*s_size*3
              end if
            end do
          else
            ! -------------
            ! wait for the receive comms and update min/max values
            do p=1,r_proc_nb
              call spmd_waitany(r_proc_nb,r_req,ijk)
              proc_id = r_index(ijk) ! get the R processor id
              r_address = 1 + 2*(iad_elem(1,proc_id) - iad_elem(1,1))*3
              do j=iad_elem(1,proc_id),iad_elem(1,proc_id+1)-1
                node_id = fr_elem(j)
                min_value(node_id,1) = min(min_value(node_id,1), r_buffer(r_address))
                max_value(node_id,1) = max(max_value(node_id,1), r_buffer(r_address+1))
                min_value(node_id,2) = min(min_value(node_id,2), r_buffer(r_address+2))
                max_value(node_id,2) = max(max_value(node_id,2), r_buffer(r_address+3))
                min_value(node_id,3) = min(min_value(node_id,3), r_buffer(r_address+4))
                max_value(node_id,3) = max(max_value(node_id,3), r_buffer(r_address+5))
                r_address = r_address + 6
              end do
            end do
            ! -------------

            ! -------------
            ! wait for the send comms
            call spmd_waitall(s_proc_nb,s_req)
            ! -------------
          endif
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine spmd_exch_min_max
      end module spmd_exch_min_max_mod