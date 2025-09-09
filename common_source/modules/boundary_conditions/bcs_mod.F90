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
!||    bcs_mod                  ../common_source/modules/boundary_conditions/bcs_mod.F90
!||--- called by ------------------------------------------------------
!||    alemain                  ../engine/source/ale/alemain.F
!||    bcs_wall_trigger         ../engine/source/boundary_conditions/bcs_wall_trigger.F90
!||    contrl                   ../starter/source/starter/contrl.F
!||    ddsplit                  ../starter/source/restart/ddsplit/ddsplit.F
!||    hm_read_bcs_wall         ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
!||    init_bcs_wall            ../starter/source/boundary_conditions/init_bcs_wall.F90
!||    initia                   ../starter/source/elements/initia/initia.F
!||    lectur                   ../engine/source/input/lectur.F
!||    rdresb                   ../engine/source/output/restart/rdresb.F
!||    read_bcs_wall            ../engine/source/output/restart/read_bcs_wall.F90
!||    resol                    ../engine/source/engine/resol.F
!||    split_bcs_wall           ../starter/source/restart/ddsplit/split_bcs_wall.F90
!||    st_qaprint_constraints   ../starter/source/output/qaprint/st_qaprint_constraints.F
!||    w_bcs_proc               ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||    write_bcs_wall           ../common_source/output/restart/write_bcs_wall.F90
!||    wrrestp                  ../engine/source/output/restart/wrrestp.F
!||--- uses       -----------------------------------------------------
!||    precision_mod            ../common_source/modules/precision_mod.F90
!||====================================================================
      module bcs_mod
        use precision_mod, only : WP
        !use constant_mod , only : zero, ep20
        implicit none
        private :: WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        ! specific to /BCS/WALL and /BCS/NRF
        type bcs_face_data_
          integer :: size
          integer, dimension(:), allocatable :: elem
          integer, dimension(:), allocatable :: face
          integer, dimension(:), allocatable :: adjacent_elem
          real(kind=WP), dimension(:) ,allocatable ::  rCp
          real(kind=WP), dimension(:) ,allocatable ::  rCs
        end type bcs_face_data_

         ! specific to /BCS/NRF
        type bcs_nrf_struct_
          integer :: user_id = 0
          integer :: set_id = 0
          type (bcs_face_data_) :: list
        end type bcs_nrf_struct_

        ! specific to /BCS/WALL
        type bcs_wall_struct_
          logical :: is_enabled = .false.
          logical :: is_depending_on_time = .false.
          logical :: is_depending_on_sensor = .false.
          real(kind=WP) :: tstart
          real(kind=WP) :: tstop
          integer :: user_id = 0
          integer :: grnod_id = 0
          integer :: sensor_id = 0
          type (bcs_face_data_) :: list
        end type bcs_wall_struct_

        !GENERAL DATA STRUCTURE /BCS
        type bcs_struct_
          integer :: num_wall
          integer :: num_nrf
          type(bcs_wall_struct_),dimension(:),allocatable :: wall
          type(bcs_nrf_struct_),dimension(:),allocatable :: nrf
          integer, allocatable, dimension(:,:) :: iworking_array
        contains
          procedure :: deallocate
        end type bcs_struct_

! ----------------------------------------------------------------------------------------------------------------------

        type(bcs_struct_) :: bcs

! ----------------------------------------------------------------------------------------------------------------------
      contains

! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Deallocate related data structure if allocated
!||====================================================================
!||    deallocate   ../common_source/modules/boundary_conditions/bcs_mod.F90
!||====================================================================
        subroutine deallocate(this)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          class(bcs_struct_),intent(inout) :: this
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if(this%num_wall > 0)then
            do ii=1,this%num_wall
              if(this%wall(ii)%list%size > 0)then
                if(allocated(this%wall(ii)%list%elem)) deallocate(this%wall(ii)%list%elem)
                if(allocated(this%wall(ii)%list%face)) deallocate(this%wall(ii)%list%face)
                if(allocated(this%wall(ii)%list%adjacent_elem)) deallocate(this%wall(ii)%list%adjacent_elem)
              end if
            end do
            if(allocated(this%wall))deallocate(this%wall)
          end if

          if(this%num_nrf > 0)then
            do ii=1,this%num_nrf
              if(this%nrf(ii)%list%size > 0)then
                if(allocated(this%nrf(ii)%list%elem)) deallocate(this%nrf(ii)%list%elem)
                if(allocated(this%nrf(ii)%list%face)) deallocate(this%nrf(ii)%list%face)
                if(allocated(this%nrf(ii)%list%adjacent_elem)) deallocate(this%nrf(ii)%list%adjacent_elem)
                if(allocated(this%nrf(ii)%list%rCp)) deallocate(this%nrf(ii)%list%rCp)
                if(allocated(this%nrf(ii)%list%rCs)) deallocate(this%nrf(ii)%list%rCs)
              end if
            end do
            if(allocated(this%nrf))deallocate(this%nrf)
          end if

          if(allocated(this%iworking_array))deallocate(this%iworking_array)
! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine deallocate



      end module bcs_mod
