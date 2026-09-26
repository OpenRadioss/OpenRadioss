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
!||    read_bcs_nrf_cfl_mod   ../engine/source/output/restart/read_bcs_nrf_cfl.F90
!||--- called by ------------------------------------------------------
!||    rdresb                 ../engine/source/output/restart/rdresb.F
!||====================================================================
      module read_bcs_nrf_cfl_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Read buffer for restart file.
!! \details  necessary buffer specific to option /BCS/NRF/...
!
!||====================================================================
!||    read_bcs_nrf_cfl   ../engine/source/output/restart/read_bcs_nrf_cfl.F90
!||--- called by ------------------------------------------------------
!||    rdresb             ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_i_c           ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    bcs_mod            ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    my_alloc_mod       ../common_source/tools/memory/my_alloc.F90
!||====================================================================
        subroutine read_bcs_nrf_cfl(nspmd)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs
          use constant_mod, only : zero
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nspmd !< number of processors
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,my_size
          integer, dimension(nspmd) :: itmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          ! ---------------
          if(bcs%num_nrf > 0)then
            ! ---------------
            ! List of nodes with BCS/NRF contributions on this processor          
            call read_i_c(itmp,1)
            bcs%nrf_num_nodes = itmp(1)
            if(.not.allocated(bcs%nrf_node_ids)) allocate(bcs%nrf_node_ids(bcs%nrf_num_nodes))
            call read_i_c(bcs%nrf_node_ids,bcs%nrf_num_nodes)
            ! ---------------
              
            ! ---------------
            ! cfl conditions for /BCS/NRF:
            ! -------
            ! Address of nodal contributions: %nod_iadsky array            
            call read_i_c(itmp,1)
            bcs%cfl_nrf%s_nod_iadsky = itmp(1) ! size of %nod_iadsky array
            if(.not.allocated(bcs%cfl_nrf%nod_iadsky)) allocate(bcs%cfl_nrf%nod_iadsky(bcs%cfl_nrf%s_nod_iadsky))
            call read_i_c(bcs%cfl_nrf%nod_iadsky,bcs%cfl_nrf%s_nod_iadsky)
            ! -------

            ! cfl condition:
            ! -------
            ! Size of %send_iadfsky & %rcv_iadfsky arrays
            if(.not.allocated(bcs%cfl_nrf%ddm)) allocate(bcs%cfl_nrf%ddm(nspmd))          
            call read_i_c(itmp,nspmd)
            do i=1,nspmd
              bcs%cfl_nrf%ddm(i)%s_cont_nb = itmp(i) ! number of contributions (send) --> size of %send_iadfsky array
            enddo
            call read_i_c(itmp,nspmd)
            do i=1,nspmd
              bcs%cfl_nrf%ddm(i)%r_cont_nb = itmp(i) ! number of contributions (receive) --> size of %rcv_iadfsky array
            enddo
            ! -------

            ! -------
            ! %send_iadfsky & %rcv_iadfsky arrays: address of sending/receiving contributions                
            do i=1,nspmd
              my_size = bcs%cfl_nrf%ddm(i)%s_cont_nb
              if(.not.allocated(bcs%cfl_nrf%ddm(i)%send_iadfsky)) allocate(bcs%cfl_nrf%ddm(i)%send_iadfsky(my_size))
              call read_i_c(bcs%cfl_nrf%ddm(i)%send_iadfsky,my_size) ! number of contributions (send) --> size of %send_iadfsky array
              my_size = bcs%cfl_nrf%ddm(i)%r_cont_nb
              if(.not.allocated(bcs%cfl_nrf%ddm(i)%rcv_iadfsky)) allocate(bcs%cfl_nrf%ddm(i)%rcv_iadfsky(my_size))
              call read_i_c(bcs%cfl_nrf%ddm(i)%rcv_iadfsky,my_size) ! number of contributions (receive) --> size of %rcv_iadfsky array
            end do
            ! -------

            ! -------
            ! Size of %iadsky array & %iadsky array: address of nodalcontributions
            call read_i_c(itmp,1)
            bcs%cfl_nrf%s_iadsky = itmp(1)
            my_size = 4*bcs%cfl_nrf%s_iadsky
            if(.not.allocated(bcs%cfl_nrf%iadsky)) allocate(bcs%cfl_nrf%iadsky(4,bcs%cfl_nrf%s_iadsky))
            call read_i_c(bcs%cfl_nrf%iadsky,my_size)
            ! -------

            ! -------
            ! Size of %fsky array
            call read_i_c(itmp,1)
            bcs%cfl_nrf%s_fsky = itmp(1)
            if(.not.allocated(bcs%cfl_nrf%fsky)) allocate(bcs%cfl_nrf%fsky(3,bcs%cfl_nrf%s_fsky))
            bcs%cfl_nrf%fsky(1:3,1:bcs%cfl_nrf%s_fsky) = zero
          end if
          ! ---------------

! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine read_bcs_nrf_cfl
      end module read_bcs_nrf_cfl_mod
