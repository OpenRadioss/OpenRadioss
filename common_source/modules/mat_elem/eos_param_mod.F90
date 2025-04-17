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
      !||    eos_param_mod             ../common_source/modules/mat_elem/eos_param_mod.F90
      !||--- called by ------------------------------------------------------
      !||    compaction2               ../common_source/eos/compaction2.F90
      !||    hm_read_eos_compaction2   ../starter/source/materials/eos/hm_read_eos_compaction2.F90
      !||    matparam_def_mod          ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    read_eosparam             ../engine/source/output/restart/read_eosparam.F90
      !||    write_eosparam            ../engine/source/output/restart/write_eosparam.F90
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod      ../common_source/modules/names_and_titles_mod.F
      !||    table4d_mod               ../common_source/modules/table4d_mod.F
      !||====================================================================
      module eos_param_mod

! ======================================================================================================================
!! \brief module to define data structure for viscosity model parameters in materials
!! \details 

      use table4d_mod
      use names_and_titles_mod

      implicit none
!
#include "my_real.inc"
!
!=======================================================================      
      
      type eos_param_
        character(len=nchartitle) :: title = ''  !< eos model input name
        integer :: nuparam                       !< number of real value paraameters
        integer :: niparam                       !< number of int value parameters
        integer :: nuvar                         !< number of internal state variables
        integer :: nfunc                         !< number of local functions in material
        integer :: ntable                        !< number of local function tables
        integer :: isfluid                      !< indicated if EoS is designed for fluid
        my_real :: cv                            !< specific heat capacity (constant volume)
        my_real :: cp                            !< specific heat capacity (constant pressure)

        my_real        ,dimension(:) ,allocatable :: uparam  !< real value eos parameter table
        integer        ,dimension(:) ,allocatable :: iparam  !< int  value eos parameter table
        integer        ,dimension(:) ,allocatable :: func    !< function table in eos models
        type(table_4d_),dimension(:) ,allocatable :: table   !< local function tables


        contains
          procedure :: destruct => destruct_eos_param
          procedure :: construct => construct_eos_param

      end type eos_param_

      contains

      !||====================================================================
      !||    destruct_eos_param   ../common_source/modules/mat_elem/eos_param_mod.F90
      !||====================================================================
        subroutine destruct_eos_param(this)
          implicit none
          class(eos_param_) ,intent(inout) :: this
          if(allocated(this%uparam)) deallocate(this%uparam)
          if(allocated(this%iparam)) deallocate(this%iparam)
          if(allocated(this%func))   deallocate(this%func)
          if(allocated(this%table))  deallocate(this%table)
        end subroutine destruct_eos_param

      !||====================================================================
      !||    construct_eos_param   ../common_source/modules/mat_elem/eos_param_mod.F90
      !||====================================================================
        subroutine construct_eos_param(this)
          implicit none
          class(eos_param_) ,intent(inout) :: this
          if(.not. allocated(this%uparam) .and.  this%nuparam >= 0) allocate(this%uparam(this%nuparam))
          if(.not. allocated(this%iparam) .and.  this%niparam >= 0) allocate(this%iparam(this%niparam))
          if(.not. allocated(this%func) .and.  this%nfunc >= 0)   allocate(this%func(this%nfunc))
          if(.not. allocated(this%table) .and.  this%ntable >= 0)  allocate(this%table(this%ntable))
        end subroutine construct_eos_param
!
!---------------
      end module eos_param_mod
