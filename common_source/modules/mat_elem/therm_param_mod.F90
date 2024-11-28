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

      !||====================================================================
      !||    therm_param_mod         ../common_source/modules/mat_elem/therm_param_mod.F90
      !||--- called by ------------------------------------------------------
      !||    matparam_def_mod       ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
      module therm_param_mod

! ======================================================================================================================
!! \brief module to define data structure for thermal model parameters in materials
!! \details 


!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"
!
!=======================================================================      
      
      type therm_param_
        integer     :: iform       !< heat transfer formulation flag 
                                   !<    iform = 0 => finite volume method
        integer     :: func_thexp  !< thermal expansion coefficient function
        my_real     :: tref        !< reference temperature
        my_real     :: tmelt       !< melting   temperature
        my_real     :: rhocp       !< specific heat per volume unit
        my_real     :: as          !< thermal conductivity coefficient A for solid phase
        my_real     :: bs          !< thermal conductivity coefficient B for solid phase 
        my_real     :: al          !< thermal conductivity coefficient A for liquid phase
        my_real     :: bl          !< thermal conductivity coefficient B for liquid phase
        my_real     :: efrac       !< energy fraction used as a heat source
        my_real     :: scale_thexp !< scale factor for thermal expansion function
      
      end type therm_param_   
!
!---------------
      end module therm_param_mod
