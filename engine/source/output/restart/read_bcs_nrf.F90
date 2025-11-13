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
! ======================================================================================================================
!||====================================================================
!||    read_bcs_nrf_mod   ../engine/source/output/restart/read_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    rdresb             ../engine/source/output/restart/rdresb.F
!||====================================================================
      module read_bcs_nrf_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Read buffer for restart file.
!! \details  necessary buffer specific to option /BCS/NRF/...
!
!||====================================================================
!||    read_bcs_nrf    ../engine/source/output/restart/read_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    rdresb          ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_db         ../common_source/tools/input_output/read_db.F
!||    read_i_c        ../common_source/tools/input_output/write_routines.c
!||--- uses       -----------------------------------------------------
!||    bcs_mod         ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine read_bcs_nrf()
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(3) :: itmp
          integer :: ilen,ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if(bcs%num_nrf > 0)then

            allocate(bcs%nrf(bcs%num_nrf))

            do ii=1,bcs%num_nrf
              call read_i_c(itmp,3)
              bcs%nrf(ii)%user_id   = itmp(1)
              bcs%nrf(ii)%set_id    = itmp(2)
              bcs%nrf(ii)%list%size = itmp(3)

              ilen = itmp(3)
              if(ilen > 0)then
                allocate(bcs%nrf(ii)%list%elem(ilen)) ; call read_i_c(bcs%nrf(ii)%list%elem(1),ilen)
                allocate(bcs%nrf(ii)%list%face(ilen)) ; call read_i_c(bcs%nrf(ii)%list%face(1),ilen)
                allocate(bcs%nrf(ii)%list%rCp(ilen)) ; call read_db(bcs%nrf(ii)%list%rCp(1),ilen)
                allocate(bcs%nrf(ii)%list%rCs(ilen)) ; call read_db(bcs%nrf(ii)%list%rCs(1),ilen)
              end if

            end do
          end if

! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine read_bcs_nrf
      end module read_bcs_nrf_mod
