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
      !||    material_is_high_explosive_mod   ../starter/source/materials/material_is_high_explosive.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_init_mixture_vel         ../starter/source/initial_conditions/detonation/eikonal_ini_mixture_vel.F90
      !||    read_dfs_detpoint                ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||====================================================================
      module material_is_high_explosive_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Return true if material law requires computaiton of detonation times
!! \details return TRUE only with mlw==5 (JWL) or mlw==97 (JWLB)
!! \details other material law are high explosive models but do not require time control (law41 :Lee-Tarver,...)
      !||====================================================================
      !||    material_is_high_explosive   ../starter/source/materials/material_is_high_explosive.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_init_mixture_vel     ../starter/source/initial_conditions/detonation/eikonal_ini_mixture_vel.F90
      !||    read_dfs_detpoint            ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||====================================================================
        logical function material_is_high_explosive(mlw)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
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
          integer,intent(in) :: mlw          !< material law type
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          material_is_high_explosive = .false.
          
          if(mlw == 5 .or. mlw == 97)then !jwl and jwlb
            material_is_high_explosive = .true.
          endif

! ----------------------------------------------------------------------------------------------------------------------
        end function material_is_high_explosive
      end module material_is_high_explosive_mod
