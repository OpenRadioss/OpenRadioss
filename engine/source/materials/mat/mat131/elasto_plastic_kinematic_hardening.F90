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
!||    elasto_plastic_kinematic_hardening_mod   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                              ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                              ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells                     ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids                     ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                              ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                              ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||====================================================================
      module elasto_plastic_kinematic_hardening_mod
! \brief Compute elasto-plastic kinematic hardening for /MAT/LAW131
! \details Compute the kinematic hardening contribution (backstress update)
!          for /MAT/LAW131 (elasto-plastic material law).
      contains
!||====================================================================
!||    elasto_plastic_kinematic_hardening   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||--- called by ------------------------------------------------------
!||    cppm_shells                          ../engine/source/materials/mat/mat131/return_mapping/cppm_shells.F90
!||    cppm_solids                          ../engine/source/materials/mat/mat131/return_mapping/cppm_solids.F90
!||    cutting_plane_shells                 ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_shells.F90
!||    cutting_plane_solids                 ../engine/source/materials/mat/mat131/return_mapping/cutting_plane_solids.F90
!||    nice_shells                          ../engine/source/materials/mat/mat131/return_mapping/nice_shells.F90
!||    nice_solids                          ../engine/source/materials/mat/mat131/return_mapping/nice_solids.F90
!||--- calls      -----------------------------------------------------
!||    kinematic_hardening_chaboche         ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_chaboche.F90
!||    kinematic_hardening_prager           ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_prager.F90
!||--- uses       -----------------------------------------------------
!||    kinematic_hardening_chaboche_mod     ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_chaboche.F90
!||    kinematic_hardening_prager_mod       ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_prager.F90
!||    matparam_def_mod                     ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                        ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine elasto_plastic_kinematic_hardening(                           &
          matparam ,nel      ,l_sigb   ,dsigb_dlam,dsigy_dpla,chard    ,       &
          normxx   ,normyy   ,normzz   ,normxy    ,normyz    ,normzx   ,       &
          dpla_dlam,sigb     ,ikine    )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use precision_mod, only : WP
        use kinematic_hardening_prager_mod
        use kinematic_hardening_chaboche_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),        intent(in)    :: matparam   !< Material parameters data
        integer,                       intent(in)    :: nel        !< Number of elements in the group
        integer,                       intent(in)    :: l_sigb     !< Number of backstress components
        real(kind=WP),dimension(nel,l_sigb),intent(inout) :: dsigb_dlam !< Backstress components for kinematic hardening
        real(kind=WP), dimension(nel), intent(in)    :: dsigy_dpla !< Derivative of yield stress wrt equivalent plastic strain
        real(kind=WP),                 intent(in)    :: chard      !< Mixed hardening parameter
        real(kind=WP), dimension(nel), intent(in)    :: normxx     !< 1st derivative of equivalent stress wrt stress xx
        real(kind=WP), dimension(nel), intent(in)    :: normyy     !< 1st derivative of equivalent stress wrt stress yy
        real(kind=WP), dimension(nel), intent(in)    :: normzz     !< 1st derivative of equivalent stress wrt stress zz
        real(kind=WP), dimension(nel), intent(in)    :: normxy     !< 1st derivative of equivalent stress wrt stress xy
        real(kind=WP), dimension(nel), intent(in)    :: normyz     !< 1st derivative of equivalent stress wrt stress yz
        real(kind=WP), dimension(nel), intent(in)    :: normzx     !< 1st derivative of equivalent stress wrt stress zx
        real(kind=WP), dimension(nel), intent(in)    :: dpla_dlam  !< Derivative of equivalent plastic strain w.r.t plastic multiplier
        real(kind=WP), dimension(nel,l_sigb),intent(in) :: sigb    !< Backstress components for kinematic hardening
        integer,                       intent(in)    :: ikine      !< Kinematic hardening type
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: offset
!===============================================================================
!
        !=======================================================================
        !< - Select kinematic hardening model
        !=======================================================================
        offset = matparam%iparam(27) + 1
        select case(ikine)
          !---------------------------------------------------------------------
          !< Prager kinematic hardening model
          !---------------------------------------------------------------------
          case(1)
            call kinematic_hardening_prager(                                   &
              nel      ,l_sigb   ,dsigb_dlam,dsigy_dpla,chard    ,             &
              normxx   ,normyy   ,normzz   ,normxy    ,normyz    ,normzx   )
          !---------------------------------------------------------------------
          !< Chaboche-Rousselier kinematic hardening model
          !---------------------------------------------------------------------
          case(2)
            call kinematic_hardening_chaboche(                                 &
              matparam ,nel      ,l_sigb   ,dsigb_dlam,sigb      ,chard    ,   &
              normxx   ,normyy   ,normzz   ,normxy    ,normyz    ,normzx   ,   &
              dpla_dlam,offset   )
        end select
!
      end subroutine elasto_plastic_kinematic_hardening
      end module elasto_plastic_kinematic_hardening_mod
