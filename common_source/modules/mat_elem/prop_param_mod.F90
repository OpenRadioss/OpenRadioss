!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    prop_param_mod               ../common_source/modules/mat_elem/prop_param_mod.F90
!||--- called by ------------------------------------------------------
!||    m1law                        ../engine/source/materials/mat/mat001/m1law.F
!||    m1lawi                       ../engine/source/materials/mat/mat001/m1lawi.F
!||    m1lawtot                     ../engine/source/materials/mat/mat001/m1lawtot.F
!||    m22law                       ../engine/source/materials/mat/mat022/m22law.F
!||    m24law                       ../engine/source/materials/mat/mat024/m24law.F
!||    m2law                        ../engine/source/materials/mat/mat002/m2law.F
!||    m46law                       ../engine/source/materials/mat/mat046/m46law.F
!||    mat_elem_mod                 ../common_source/modules/mat_elem/mat_elem_mod.F90
!||    mulaw                        ../engine/source/materials/mat_share/mulaw.F90
!||    s6zrcoor3                    ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||    sboltlaw                     ../engine/source/elements/solid/solide/sboltlaw.F
!||    sigeps51                     ../engine/source/materials/mat/mat051/sigeps51.F90
!||    sigeps51_boundary_material   ../engine/source/materials/mat/mat051/sigeps51_boundary_material.F90
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod         ../common_source/modules/names_and_titles_mod.F
!||    ply_param_mod                ../common_source/modules/mat_elem/ply_param_mod.F90
!||    precision_mod                ../common_source/modules/precision_mod.F90
!||====================================================================
      module prop_param_mod

!=======================================================================================
!! \brief  module to define element property data structure
!! \details

        use ply_param_mod
        use names_and_titles_mod, only: nchartitle
        use precision_mod, only: WP


        implicit none
        private :: WP
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------

        integer ,parameter  :: n_var_iparg = 100    ! number of iparg variables = nparg
        integer ,parameter  :: n_var_pm    = 250    ! number of pm variables    = npropm
        integer ,parameter  :: n_var_ipm   = 467    ! number of ipm variables   = npropmi
        integer ,parameter  :: n_var_igeo  = 917    ! number of igeo variables  = npropgi
        integer ,parameter  :: n_var_geo   = 1000   ! number of geo variables   = npropg
!-----------------------------------------------------------------------
!
        type prop_param_                      ! (numgeo)
          character(len=nchartitle) :: title  ! Property title
          integer     :: prop_id
          integer     :: nlay

          type (ply_param_) ,dimension(:) ,allocatable :: ply_param !< nlay

          integer  ,dimension(:) ,allocatable :: igeo               !< npropg
          real(kind=WP)  ,dimension(:) ,allocatable :: geo                !< npropgi

        end type prop_param_
!
!---------------
      end module prop_param_mod
