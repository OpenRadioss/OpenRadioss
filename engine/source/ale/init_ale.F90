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
!||    init_ale_mod   ../engine/source/ale/init_ale.F90
!||--- called by ------------------------------------------------------
!||    resol          ../engine/source/engine/resol.F
!||====================================================================
      module init_ale_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief
!! \details
!||====================================================================
!||    init_ale   ../engine/source/ale/init_ale.F90
!||--- called by ------------------------------------------------------
!||    resol      ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    ale_mod    ../common_source/modules/ale/ale_mod.F
!||====================================================================
        subroutine init_ale(global_active_ale_element,n2d,numels,numelq,nmult,iale,ieuler,trimat,itherm,ale)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ale_mod , only : ale_
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
          logical, intent(in) :: global_active_ale_element !< global flag for ALE element activation
          integer, intent(in) :: n2d !< 0: 3D, 1: 2D
          integer, intent(in) :: numels !< number of solid elements
          integer, intent(in) :: numelq !< number of quad elements
          integer, intent(in) :: nmult !< number of ALE materials (2d case)
          integer, intent(in) :: iale !< ALE activated flag
          integer, intent(in) :: ieuler !< Eulerian activated flag
          integer, intent(in) :: trimat !< number of sub-materials
          integer, intent(in) :: itherm !< thermal activated flag

          type(ale_), intent(inout) :: ale !< ALE data structure

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if(n2d==0) then
            ale%global%nv46 = 6
          else
            ale%global%nv46 = 4
          end if
          ale%global%s_flux = 0
          ale%global%s_qmv = 0
          if(iale+ieuler+itherm/=0.and.global_active_ale_element) then
            if(n2d==0) then
              ale%global%s_flux = numels+numelq
            else
              ale%global%s_flux = max(1,nmult)*4*numelq
            end if

            ale%global%s_qmv = 1
            if(trimat>0) ale%global%s_qmv = min(1,trimat)*(numels+numelq)
          end if

          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_ale
      end module init_ale_mod
