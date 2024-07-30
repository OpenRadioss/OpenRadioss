!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
!Chd|====================================================================
!Chd|  ply_param_mod                 modules/mat_elem/ply_param_mod.f
!Chd|-- called by -----------
!Chd|        prop_param_mod                common_source/modules/mat_elem/prop_param_mod.F
!Chd|-- calls ---------------
!Chd|====================================================================

      !||====================================================================
      !||    ply_param_mod    ../common_source/modules/mat_elem/ply_param_mod.F90
      !||--- called by ------------------------------------------------------
      !||    prop_param_mod   ../common_source/modules/mat_elem/prop_param_mod.F90
      !||====================================================================
      module ply_param_mod

!=======================================================================================      
!! \brief  module to define composite ply data structure
!! \details 
!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"

!=======================================================================      

      type ply_param_                    
        my_real :: alpha                
        my_real :: phi
        my_real :: thick
        my_real :: pos
        my_real :: p_thkly
        my_real :: p_weight
        integer :: pid
        integer :: mid_ply
        integer :: mid_intply
        integer :: npt_ply
      end type ply_param_

!---------------
      end module ply_param_mod
