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
!||    read_bcs_wall_mod   ../engine/source/output/restart/read_bcs_wall.F90
!||--- called by ------------------------------------------------------
!||    rdresb              ../engine/source/output/restart/rdresb.F
!||====================================================================
      module read_bcs_wall_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Read buffer for restart file.
!! \details  necessary buffer specific to option /BCS/WALL/...
!
!||====================================================================
!||    read_bcs_wall   ../engine/source/output/restart/read_bcs_wall.F90
!||--- called by ------------------------------------------------------
!||    rdresb          ../engine/source/output/restart/rdresb.F
!||--- calls      -----------------------------------------------------
!||    read_db         ../common_source/tools/input_output/read_db.F
!||    read_i_c        ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    bcs_mod         ../common_source/modules/boundary_conditions/bcs_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine read_bcs_wall()
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
          real(kind=WP), dimension(2) :: rtmp
          integer, dimension(7) :: itmp
          integer :: ilen,ii
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if(bcs%num_wall > 0)then

            allocate(bcs%wall(bcs%num_wall))

            do ii=1,bcs%num_wall
              call read_i_c(itmp,7)
              bcs%wall(ii)%is_enabled             = (itmp(1)==1)
              bcs%wall(ii)%is_depending_on_time   = (itmp(2)==1)
              bcs%wall(ii)%is_depending_on_sensor = (itmp(3)==1)
              bcs%wall(ii)%user_id                = itmp(4)
              bcs%wall(ii)%grnod_id               = itmp(5)
              bcs%wall(ii)%sensor_id              = itmp(6)
              bcs%wall(ii)%list%size              = itmp(7)

              ilen = itmp(7)
              if(ilen > 0)then
                allocate(bcs%wall(ii)%list%elem(ilen)) ; call read_i_c(bcs%wall(ii)%list%elem(1),ilen)
                allocate(bcs%wall(ii)%list%face(ilen)) ; call read_i_c(bcs%wall(ii)%list%face(1),ilen)
                allocate(bcs%wall(ii)%list%adjacent_elem(ilen)) ; call read_i_c(bcs%wall(ii)%list%adjacent_elem(1),ilen)
              endif

              call read_db(rtmp,2)
              bcs%wall(ii)%tstart = rtmp(1)
              bcs%wall(ii)%tstop = rtmp(2)

            enddo
          endif

! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine read_bcs_wall
      end module read_bcs_wall_mod
