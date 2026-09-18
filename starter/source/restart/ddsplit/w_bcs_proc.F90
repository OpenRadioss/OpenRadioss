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
!||    w_bcs_proc_mod   ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||--- called by ------------------------------------------------------
!||    ddsplit          ../starter/source/restart/ddsplit/ddsplit.F
!||====================================================================
      module w_bcs_proc_mod
        implicit none
      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Data pre-treatment before saving in RESTART FILE
!! \details  necessary buffer specific to option /BCS/WALL/, /BCS/NRF , ...
!
!||====================================================================
!||    w_bcs_proc           ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||--- called by ------------------------------------------------------
!||    ddsplit              ../starter/source/restart/ddsplit/ddsplit.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine w_bcs_proc(bcs_per_proc,cel,scel,len_ia,len_am,nodlocal,numnod_g,nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs_struct_
          use write_bcs_wall_mod , only : write_bcs_wall
          use write_bcs_nrf_mod , only : write_bcs_nrf
          use write_bcs_nrf_cfl_mod , only : write_bcs_nrf_cfl

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: scel                         !< size for array definition
          integer, intent(in) :: nspmd !< number of processors
          integer,intent(in),dimension(scel) :: cel          !< application : global_elem_id -> local_elem_id
          type(bcs_struct_),intent(inout) :: bcs_per_proc    !< local data structure for bcs
          integer,intent(inout) :: len_ia,len_am             !< buffer size for records (integer and real)
          integer,intent(in) :: numnod_g                     !< number of nodes in global mesh
          integer,intent(in),dimension(numnod_g) :: nodlocal !< application : global_node_id -> local_node_id
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(1) :: itmp
          integer :: ilen,ii,jj,ielem,inod,kk,i,my_size
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          !-------------------------------------
          !        /BCS/WALL
          !-------------------------------------
          itmp(1) = bcs_per_proc%num_wall
          call write_i_c(itmp,1)
          len_ia = len_ia + 1
          if(bcs_per_proc%num_wall > 0)then
            do ii=1,bcs_per_proc%num_wall
              ilen = bcs_per_proc%wall(ii)%list%size
              if(ilen > 0)then
                do jj=1, ilen
                  ielem = bcs_per_proc%wall(ii)%list%elem(jj)
                  bcs_per_proc%wall(ii)%list%elem(jj) = cel(ielem) !local numbering
                end do
              end if
              call write_bcs_wall(bcs_per_proc%wall(ii))
              len_ia = len_ia + 7 + 3*ilen
              len_am = len_am + 2
            end do!next ii
          end if

          !-------------------------------------
          !        /BCS/NRF
          !-------------------------------------
          itmp(1) = bcs_per_proc%num_nrf
          call write_i_c(itmp,1)
          len_ia = len_ia + 1
          if(bcs_per_proc%num_nrf > 0)then
            do ii=1,bcs_per_proc%num_nrf
              ilen = bcs_per_proc%nrf(ii)%list%size
              if(ilen > 0)then
                do jj=1, ilen
                  ielem = bcs_per_proc%nrf(ii)%list%elem(jj)
                  bcs_per_proc%nrf(ii)%list%elem(jj) = cel(ielem) !local numbering
                  do kk=1,4
                    inod = bcs_per_proc%nrf(ii)%list%node_list(kk,jj)
                    if(inod > 0)then
                      bcs_per_proc%nrf(ii)%list%node_list(kk,jj) = nodlocal(inod) !local numbering
                    end if
                  end do
                end do
              end if
              call write_bcs_nrf(bcs_per_proc%nrf(ii))
              len_ia = len_ia + 3 + 10*ilen
              len_am = len_am + 2*ilen          
            end do!next ii

            !-------------
            ! CFL conditions for /BCS/NRF:
            do i=1,bcs_per_proc%nrf_num_nodes
              bcs_per_proc%nrf_node_ids(i) = nodlocal(bcs_per_proc%nrf_node_ids(i)) ! convert to the local node id
            end do

            call write_bcs_nrf_cfl(bcs_per_proc,nspmd)
            len_ia = len_ia + 2
            len_ia = len_ia + bcs_per_proc%nrf_num_nodes + bcs_per_proc%cfl_nrf%s_nod_iadsky
            len_ia = len_ia + 2*nspmd
            my_size = 0
            do i=1,nspmd
              my_size = my_size + bcs_per_proc%cfl_nrf%ddm(i)%s_cont_nb
              my_size = my_size + bcs_per_proc%cfl_nrf%ddm(i)%r_cont_nb
            end do
            len_ia = len_ia + my_size   
            ! Size of %iadsky array & %iadsky array: address of nodal contributions
            my_size = 4*bcs_per_proc%cfl_nrf%s_iadsky
            len_ia = len_ia + my_size + 1 + 1
            deallocate(bcs_per_proc%cfl_nrf%nod_iadsky)
            deallocate(bcs_per_proc%nrf_node_ids)
            !-------------            
          end if

! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine w_bcs_proc


! ======================================================================================================================
      end module w_bcs_proc_mod
