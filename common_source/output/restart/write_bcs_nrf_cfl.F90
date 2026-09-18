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
! ======================================================================================================================
!||====================================================================
!||    write_bcs_nrf_mod   ../common_source/output/restart/write_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    w_bcs_proc          ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||    wrrestp             ../engine/source/output/restart/wrrestp.F
!||====================================================================
      module write_bcs_nrf_cfl_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Save buffer for restart file.
!! \details  necessary buffer specific to option /BCS/NRF/...
!
!||====================================================================
!||    write_bcs_nrf   ../common_source/output/restart/write_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    w_bcs_proc      ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||    wrrestp         ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    write_db        ../common_source/tools/input_output/write_db.F
!||    write_i_c       ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    bcs_mod         ../common_source/modules/boundary_conditions/bcs_mod.F90
!||====================================================================
        subroutine write_bcs_nrf_cfl(bcs,nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(bcs_struct_), intent(in) :: bcs !< data structure for BCS/NRF
          integer, intent(in) :: nspmd !< number of processors
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: my_size,i
          integer, dimension(nspmd) :: itmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          ! ---------------
          ! List of nodes with BCS/NRF contributions on this processor
          itmp(1) = bcs%nrf_num_nodes
          call write_i_c(itmp,1)
          call write_i_c(bcs%nrf_node_ids,bcs%nrf_num_nodes)
          ! ---------------
            
          ! ---------------
          ! cfl conditions for /BCS/NRF:
          ! -------
          ! Address of nodal contributions: %nod_iadsky array
          itmp(1) = bcs%cfl_nrf%s_nod_iadsky ! size of %nod_iadsky array
          call write_i_c(itmp,1)
          call write_i_c(bcs%cfl_nrf%nod_iadsky,bcs%cfl_nrf%s_nod_iadsky) !
          ! -------

          ! -------          
          ! Size of %send_iadfsky & %rcv_iadfsky arrays
          do i=1,nspmd
            itmp(1:nspmd) = bcs%cfl_nrf%ddm(1:nspmd)%s_cont_nb
          enddo
          call write_i_c(itmp,nspmd) ! number of contributions (send) --> size of %send_iadfsky array
          do i=1,nspmd
            itmp(1:nspmd) = bcs%cfl_nrf%ddm(1:nspmd)%r_cont_nb
          enddo          
          call write_i_c(itmp,nspmd) ! number of contributions (receive) --> size of %rcv_iadfsky array
          ! -------

          ! -------
          ! %send_iadfsky & %rcv_iadfsky arrays: address of sending/receiving contributions
          do i=1,nspmd
            my_size = bcs%cfl_nrf%ddm(i)%s_cont_nb
            call write_i_c(bcs%cfl_nrf%ddm(i)%send_iadfsky,my_size) ! number of contributions (send) --> size of %send_iadfsky array
            my_size = bcs%cfl_nrf%ddm(i)%r_cont_nb                
            call write_i_c(bcs%cfl_nrf%ddm(i)%rcv_iadfsky,my_size) ! number of contributions (receive) --> size of %rcv_iadfsky array
          end do
          ! -------

          ! -------
          ! Size of %iadsky array & %iadsky array: address of nodalcontributions
          my_size = 4*bcs%cfl_nrf%s_iadsky
          itmp(1) = bcs%cfl_nrf%s_iadsky
          call write_i_c(itmp,1)
          call write_i_c(bcs%cfl_nrf%iadsky,my_size)
          ! -------

          ! -------
          ! Size of %fsky array
          itmp(1) = bcs%cfl_nrf%s_fsky
          call write_i_c(itmp,1)
          ! -------          
          ! ---------------          


! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine write_bcs_nrf_cfl
      end module write_bcs_nrf_cfl_mod
