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
!||    eos_param_mod                ../common_source/modules/mat_elem/eos_param_mod.F90
!||--- called by ------------------------------------------------------
!||    compaction                   ../common_source/eos/compaction.F90
!||    compaction2                  ../common_source/eos/compaction2.F90
!||    compaction_tab               ../common_source/eos/compaction_tab.F90
!||    compaction_tab_init          ../common_source/eos/compaction_tab.F90
!||    contrl                       ../starter/source/starter/contrl.F
!||    eos_table_copy               ../starter/source/materials/tools/eos_table_copy.F90
!||    eosexponential               ../common_source/eos/eosexponential.F90
!||    eoslinear                    ../common_source/eos/eoslinear.F
!||    eosmain                      ../common_source/eos/eosmain.F
!||    eosmain51                    ../engine/source/materials/mat/mat051/eosmain51.F90
!||    eospolyno                    ../common_source/eos/eospolyno.F
!||    gruneisen                    ../common_source/eos/gruneisen.F
!||    hm_read_analy                ../starter/source/general_controls/computation/hm_read_analy.F
!||    hm_read_eos_compaction       ../starter/source/materials/eos/hm_read_eos_compaction.F90
!||    hm_read_eos_compaction2      ../starter/source/materials/eos/hm_read_eos_compaction2.F90
!||    hm_read_eos_compaction_tab   ../starter/source/materials/eos/hm_read_eos_compaction_tab.F90
!||    hm_read_eos_exponential      ../starter/source/materials/eos/hm_read_eos_exponential.F90
!||    hm_read_eos_powderburn       ../starter/source/materials/eos/hm_read_eos_powderburn.F90
!||    hm_read_eos_tabulated        ../starter/source/materials/eos/hm_read_eos_tabulated.F
!||    idealgas                     ../common_source/eos/idealgas.F
!||    idealgas_vt                  ../common_source/eos/idealgas_vt.F
!||    lszk                         ../common_source/eos/lszk.F
!||    m17law                       ../engine/source/materials/mat/mat017/m17law.F
!||    m6law                        ../engine/source/materials/mat/mat006/m6law.F
!||    matparam_def_mod             ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    multimat_param_mod           ../common_source/modules/multimat_param_mod.F90
!||    murnaghan                    ../common_source/eos/murnaghan.F
!||    nasg                         ../common_source/eos/nasg.F
!||    noble_abel                   ../common_source/eos/noble_abel.F
!||    osborne                      ../common_source/eos/osborne.F
!||    puff                         ../common_source/eos/puff.F
!||    rdcomi                       ../engine/source/output/restart/rdcomm.F
!||    read_eosparam                ../engine/source/output/restart/read_eosparam.F90
!||    s6zforc3                     ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||    s6zinit3                     ../starter/source/elements/solid/solide6z/s6zinit3.F90
!||    sesame                       ../common_source/eos/sesame.F
!||    stiffgas                     ../common_source/eos/stiffgas.F
!||    tabulated                    ../common_source/eos/tabulated.F
!||    tillotson                    ../common_source/eos/tillotson.F
!||    wrcomi                       ../engine/source/output/restart/wrcomm.F
!||    wrcomip                      ../starter/source/restart/ddsplit/wrcommp.F
!||    write_eosparam               ../engine/source/output/restart/write_eosparam.F90
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod         ../common_source/modules/names_and_titles_mod.F
!||    precision_mod                ../common_source/modules/precision_mod.F90
!||    table4d_mod                  ../common_source/modules/table4d_mod.F
!||====================================================================
      module eos_param_mod

! ======================================================================================================================
!! \brief module to define data structure for viscosity model parameters in materials
!! \details

        use table4d_mod
        use names_and_titles_mod
        use precision_mod, only : WP

        implicit none
        private :: WP
!
!
!=======================================================================      

      INTEGER ANALY_TEMP

      type eos_param_
        character(len=nchartitle) :: title = ''  !< eos model input name
        integer :: nuparam                       !< number of real value paraameters
        integer :: niparam                       !< number of int value parameters
        !integer :: nuvar                        !< number of internal state variables    --> elbuf%bufly%eos%var(nel*nvar_eos)bg
        integer :: nfunc                         !< number of local functions in material
        integer :: ntable                        !< number of local function tables
        integer :: isfluid                       !< indicated if EoS is designed for fluid
        integer :: eostype                       !< eos model type
        real(kind=WP) :: cv                      !< specific heat capacity (constant volume)
        real(kind=WP) :: cp                      !< specific heat capacity (constant pressure)
        real(kind=WP) :: psh                     !< pressure shift
        real(kind=WP) :: e0                      !< initial internal energy
        real(kind=WP) :: p0                      !< initial pressure
        real(kind=WP) :: pmin                    !< minimum pressure

        real(kind=WP)  ,dimension(:) ,allocatable :: uparam  !< real value eos parameter table
        integer        ,dimension(:) ,allocatable :: iparam  !< int  value eos parameter table
        integer        ,dimension(:) ,allocatable :: func    !< function table in eos models
        type(table_4d_),dimension(:) ,allocatable :: table   !< local function tables


        contains
          procedure :: destruct => destruct_eos_param
          procedure :: construct => construct_eos_param
          procedure :: copyto => copy_this_eosparam_to_target

        end type eos_param_

      type ptr_eos_param_
          ! eos parameters for each submaterials
          type(eos_param_), pointer :: eos
      end type ptr_eos_param_
      ! matparam(parent_mid)%multimat%pEOS(isubmat)%EOS(:) !with new format : => matparam(submat_mid)%eos
                                                           !with old format : allocate  (fill with embedded eos parameters)
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
          !buffer might be already allocated in cas of EoS embedded in material law (obsolete format).
          !  consequently deallocate is done if necessary
          if(this%nuparam >= 0) then
            if(allocated(this%uparam)) deallocate(this%uparam)
            allocate(this%uparam(this%nuparam))
          end if
          if(this%niparam >= 0) then
            if(allocated(this%iparam)) deallocate(this%iparam)
            allocate(this%iparam(this%niparam))
          end if
          if(this%nfunc >= 0) then
            if(allocated(this%func)) deallocate(this%func)
            allocate(this%func(this%nfunc))
          end if
          if(this%ntable >= 0) then
            if(allocated(this%table)) deallocate(this%table)
            allocate(this%table(this%ntable))
          end if
        end subroutine construct_eos_param

!||====================================================================
!||    copy_this_eosparam_to_target   ../common_source/modules/mat_elem/eos_param_mod.F90
!||====================================================================
        subroutine copy_this_eosparam_to_target(this, target)
          implicit none
          class(eos_param_) ,intent(in) :: this
          type(eos_param_) ,intent(inout) :: target
          integer :: i
          target%nuparam = this%nuparam
          target%niparam = this%niparam
          target%nfunc = this%nfunc
          target%ntable = this%ntable
          target%isfluid = this%isfluid
          target%cv = this%cv
          target%cp = this%cp
          target%psh = this%psh
          target%e0 = this%e0
          target%p0 = this%p0
          target%pmin = this%pmin
          target%title = this%title
          call target%construct()
          if(this%nuparam >= 0) then
            do i= 1,this%nuparam
              target%uparam(i) = this%uparam(i)
            end do
          end if
          if(this%niparam >= 0) then
            do i= 1,this%niparam
              target%iparam(i) = this%iparam(i)
            end do
          end if
          if(this%nfunc >= 0) then
            do i= 1,this%nfunc
              target%func(i) = this%func(i)
            end do
          end if
          if(this%ntable >= 0) then
            do i= 1,this%ntable
              call this%table(i)%copyto(target%table(i))
            end do
          end if
        end subroutine copy_this_eosparam_to_target
!
!---------------
      end module eos_param_mod
