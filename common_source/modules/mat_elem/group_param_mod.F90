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
!||--- uses       -----------------------------------------------------
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||====================================================================
      module group_param_mod

! ======================================================================================================================
!! \brief module to define data structure for common parameters in element groups
!! \details

        use precision_mod, only: WP
        implicit none
        private :: WP
!
!
!=======================================================================
!
        type group_param_               ! (ngroup)
          integer     :: imat           !< global material model number
          integer     :: iprop          !< property number
          integer     :: nparg          !< number of group parameters
          real(kind=WP)     :: visc_dn        !< numerical viscosity coefficient
          real(kind=WP)     :: visc_dm        !< membrane  viscosity coefficient (for shells)
          integer     :: ismstr         !< small/large strain formulation flag

          integer ,dimension(:) ,allocatable :: iparg  !< table of group parameters

        end type group_param_
!
!---------------
      end module group_param_mod
