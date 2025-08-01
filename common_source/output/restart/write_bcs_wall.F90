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
!||    write_bcs_wall_mod   ../common_source/output/restart/write_bcs_wall.F90
!||--- called by ------------------------------------------------------
!||    w_bcs_proc           ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||    wrrestp              ../engine/source/output/restart/wrrestp.F
!||====================================================================
     module write_bcs_wall_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Save buffer for restart file.
!! \details  necessary buffer specific to option /BCS/WALL/...
!
!||====================================================================
!||    write_bcs_wall   ../common_source/output/restart/write_bcs_wall.F90
!||--- called by ------------------------------------------------------
!||    w_bcs_proc       ../starter/source/restart/ddsplit/w_bcs_proc.F90
!||    wrrestp          ../engine/source/output/restart/wrrestp.F
!||--- calls      -----------------------------------------------------
!||    write_db         ../common_source/tools/input_output/write_db.F
!||    write_i_c        ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    bcs_mod          ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine write_bcs_wall(bcsw)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use bcs_mod , only : bcs_wall_struct_
        use precision_mod , only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        type(bcs_wall_struct_),intent(in) :: bcsw  !< global data structure for bcs
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        real(kind=WP), dimension(2) :: rtmp
        integer, dimension(7) :: itmp
        integer :: ilen
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

        ! /BCS/WALL
        !   when starting from a restart file we need to read these values

        itmp(1:3) = 0
        if(bcsw%is_enabled) itmp(1) = 1
        if(bcsw%is_depending_on_time) itmp(2) = 1
        if(bcsw%is_depending_on_sensor) itmp(3) = 1
        itmp(4) = bcsw%user_id
        itmp(5) = bcsw%grnod_id
        itmp(6) = bcsw%sensor_id
        itmp(7) = bcsw%list%size
        call write_i_c(itmp,7)

        ilen = bcsw%list%size
        if(ilen > 0)then
          call write_i_c(bcsw%list%elem,ilen)
          call write_i_c(bcsw%list%face,ilen)
          call write_i_c(bcsw%list%adjacent_elem,ilen)
        end if

        rtmp(1) = bcsw%tstart
        rtmp(2) = bcsw%tstop
        call write_db(rtmp,2)


! ----------------------------------------------------------------------------------------------------------------------
        return
      end subroutine write_bcs_wall
end module write_bcs_wall_mod
