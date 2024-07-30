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
!Chd|  group_param_mod               modules/mat_elem/group_param_mod.f
!Chd|-- called by -----------
!Chd|        mat_elem_mod                  common_source/modules/mat_elem/mat_elem_mod.f
!Chd|        c3derii                       starter/source/elements/sh3n/coque3n/c3derii.f
!Chd|        c3init3                       starter/source/elements/sh3n/coque3n/c3init3.f
!Chd|        cbainit3                      starter/source/elements/shell/coqueba/cbainit3.f
!Chd|        cdkderii                      starter/source/elements/sh3n/coquedk/cdkderii.f
!Chd|        cdkinit3                      starter/source/elements/sh3n/coquedk/cdkinit3.f
!Chd|        cdleni                        starter/source/elements/shell/coque/cdleni.f
!Chd|        cinit3                        starter/source/elements/shell/coque/cinit3.f
!Chd|        cndleni                       starter/source/elements/shell/coqueba/cndleni.f
!Chd|        inirig_mat                    starter/source/elements/initia/inirig_mat.f
!Chd|        initia                        starter/source/elements/initia/initia.f
!Chd|        outpart5                      starter/source/elements/initia/initia.f
!Chd|        set_elgroup_param             starter/source/elements/shell/coque/set_elgroup_param.f
!Chd|        write_elgroup_param           starter/source/restart/ddsplit/write_elgroup_param.f
!Chd|        cmain3pinch                   engine/source/elements/shell/coqueba/cmain3pinch.f
!Chd|        write_elgroup_param           engine/source/output/restart/write_elgroup_param.f
!Chd|-- calls ---------------
!Chd|====================================================================

      !||====================================================================
      !||    group_param_mod       ../common_source/modules/mat_elem/group_param_mod.F90
      !||--- called by ------------------------------------------------------
      !||    c3derii               ../starter/source/elements/sh3n/coque3n/c3derii.F
      !||    c3init3               ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    cbainit3              ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cdkderii              ../starter/source/elements/sh3n/coquedk/cdkderii.F
      !||    cdkinit3              ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cdleni                ../starter/source/elements/shell/coque/cdleni.F
      !||    cinit3                ../starter/source/elements/shell/coque/cinit3.F
      !||    cmain3pinch           ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    cndleni               ../starter/source/elements/shell/coqueba/cndleni.F
      !||    inirig_mat            ../starter/source/elements/initia/inirig_mat.F
      !||    initia                ../starter/source/elements/initia/initia.F
      !||    mat_elem_mod          ../common_source/modules/mat_elem/mat_elem_mod.F90
      !||    outpart5              ../starter/source/elements/initia/initia.F
      !||    set_elgroup_param     ../starter/source/elements/shell/coque/set_elgroup_param.F
      !||    write_elgroup_param   ../engine/source/output/restart/write_elgroup_param.F
      !||====================================================================
      module group_param_mod

! ======================================================================================================================
!! \brief module to define data structure for common parameters in element groups 
!! \details 

!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"
!
!=======================================================================      
! 
      type group_param_               ! (ngroup)
        integer     :: imat           !< global material model number   
        integer     :: iprop          !< property number
        integer     :: nparg          !< number of group parameters
        my_real     :: visc_dn        !< numerical viscosity coefficient
        my_real     :: visc_dm        !< membrane  viscosity coefficient (for shells)
        integer     :: ismstr         !< small/large strain formulation flag
        
        integer ,dimension(:) ,allocatable :: iparg  !< table of group parameters

      end type group_param_
!
!---------------
      end module group_param_mod
