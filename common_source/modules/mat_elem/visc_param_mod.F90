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
!Chd|====================================================================
!Chd|  visc_param_mod                modules/mat_elem/visc_param_mod.f
!Chd|-- called by -----------
!Chd|        matparam_def_mod              common_source/modules/mat_elem/matparam_def_mod.F90
!Chd|        hm_read_visc_lprony           starter/source/materials/visc/hm_read_visc_lprony.f
!Chd|        hm_read_visc_prony            starter/source/materials/visc/hm_read_visc_prony.f
!Chd|        updmat                        starter/source/materials/updmat.f
!Chd|        write_viscparam               starter/source/materials/mat/write_viscparam.f
!Chd|        read_viscparam                engine/source/output/restart/read_viscparam.f
!Chd|        viscmain                      engine/source/materials/visc/viscmain.f
!Chd|        visc_et                       engine/source/elements/solid/solidez/visc_et.f
!Chd|        visc_prony                    engine/source/materials/visc/visc_prony.f
!Chd|        visc_prony_lstrain            engine/source/materials/visc/visc_prony_lstrain.f
!Chd|        write_viscparam               engine/source/output/restart/write_viscparam.f
!Chd|-- calls ---------------
!Chd|        names_and_titles_mod          modules/names_and_titles_mod.f
!Chd|        table4d_mod                   modules/table4d_mod.f         
!Chd|====================================================================

      !||====================================================================
      !||    visc_param_mod         ../common_source/modules/mat_elem/visc_param_mod.F90
      !||--- called by ------------------------------------------------------
      !||    hm_read_visc_lprony    ../starter/source/materials/visc/hm_read_visc_lprony.F
      !||    hm_read_visc_plas      ../starter/source/materials/visc/hm_read_visc_plas.F90
      !||    hm_read_visc_prony     ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    matparam_def_mod       ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    read_viscparam         ../engine/source/output/restart/read_viscparam.F
      !||    updmat                 ../starter/source/materials/updmat.F
      !||    visc_et                ../engine/source/elements/solid/solidez/visc_et.F
      !||    visc_plas              ../engine/source/materials/visc/visc_plas.F90
      !||    visc_prony             ../engine/source/materials/visc/visc_prony.F
      !||    visc_prony_lstrain     ../engine/source/materials/visc/visc_prony_lstrain.F
      !||    viscmain               ../engine/source/materials/visc/viscmain.F
      !||    write_viscparam        ../engine/source/output/restart/write_viscparam.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
      !||    table4d_mod            ../common_source/modules/table4d_mod.F
      !||====================================================================
      module visc_param_mod

! ======================================================================================================================
!! \brief module to define data structure for viscosity model parameters in materials
!! \details 

      use table4d_mod
      use names_and_titles_mod

!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"
!
!=======================================================================      
      
      type visc_param_
        integer     :: ilaw                   !< viscosity model type (number)
        character(len=nchartitle) :: title    !< viscosity model input name
        integer     :: nuparam                !< number of real value paraameters
        integer     :: niparam                !< number of int value parameters
        integer     :: nuvar                  !< number of internal state variables
        integer     :: nfunc                  !< number of local functions in material
        integer     :: ntable                 !< number of local function tables
        
        my_real        ,dimension(:) ,allocatable :: uparam  !< real value viscosity parameter table
        integer        ,dimension(:) ,allocatable :: iparam  !< int  value viscosity parameter table
        integer        ,dimension(:) ,allocatable :: func    !< function table in viscosity models
        type(table_4d_),dimension(:) ,allocatable :: table   !< local function tables

        contains
          procedure :: destruct => destruct_visc_param

      end type visc_param_


      contains

      !||====================================================================
      !||    destruct_visc_param   ../common_source/modules/mat_elem/visc_param_mod.F90
      !||====================================================================
        subroutine destruct_visc_param(this)
          implicit none
          class(visc_param_) ,intent(inout) :: this
          if(allocated(this%uparam)) deallocate(this%uparam)
          if(allocated(this%iparam)) deallocate(this%iparam)
          if(allocated(this%func)) deallocate  (this%func)
          if(allocated(this%table)) deallocate (this%table)
        end subroutine destruct_visc_param
!
!---------------
      end module visc_param_mod
