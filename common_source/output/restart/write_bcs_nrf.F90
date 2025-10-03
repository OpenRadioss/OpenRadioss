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
!||    write_bcs_nrf_mod   ../common_source/output/restart/write_bcs_nrf.F90
!||--- called by ------------------------------------------------------
!||    w_bcs_proc          ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||    wrrestp             ../engine/source/output/restart/wrrestp.F
!||====================================================================
      module write_bcs_nrf_mod
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
!||    write_i_c       ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    bcs_mod         ../common_source/modules/boundary_conditions/bcs_mod.F90
!||====================================================================
        subroutine write_bcs_nrf(bcsnrf)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use bcs_mod , only : bcs_nrf_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(bcs_nrf_struct_),intent(in) :: bcsnrf  !< global data structure for bcs
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(3) :: itmp
          integer :: ilen
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          ! /BCS/NRF
          !   when starting from a restart file we need to read these values

          itmp(1) = bcsnrf%user_id
          itmp(2) = bcsnrf%set_id
          itmp(3) = bcsnrf%list%size
          call write_i_c(itmp,3)

          ilen = bcsnrf%list%size
          if(ilen > 0)then
            call write_i_c(bcsnrf%list%elem,ilen)
            call write_i_c(bcsnrf%list%face,ilen)
            call write_db(bcsnrf%list%rCp,ilen)
            call write_db(bcsnrf%list%rCs,ilen)
          end if

! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine write_bcs_nrf
      end module write_bcs_nrf_mod
