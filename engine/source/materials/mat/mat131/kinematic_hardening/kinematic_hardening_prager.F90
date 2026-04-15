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
!||    kinematic_hardening_prager_mod       ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_prager.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_kinematic_hardening   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||====================================================================
      module kinematic_hardening_prager_mod
! \brief Compute Prager kinematic hardening for /MAT/LAW131
! \details Compute the Prager linear kinematic hardening model
!          (backstress evolution) for /MAT/LAW131.
      contains
!||====================================================================
!||    kinematic_hardening_prager           ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_prager.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_kinematic_hardening   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                         ../common_source/modules/constant_mod.F
!||    matparam_def_mod                     ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                        ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine kinematic_hardening_prager(                                   &
        nel      ,l_sigb   ,dsigb_dlam,dsigy_dpla,chard    ,                   &
        normxx   ,normyy   ,normzz   ,normxy    ,normyz    ,normzx   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        integer,                       intent(in)    :: nel          !< Number of elements in the group
        integer,                       intent(in)    :: l_sigb       !< Number of backstress components
        real(kind=WP), dimension(nel,l_sigb),intent(inout) :: dsigb_dlam !< Backstress components derivative w.r.t plastic multiplier
        real(kind=WP),                 intent(in)    :: chard        !< Mixed hardening parameter
        real(kind=WP), dimension(nel), intent(in)    :: dsigy_dpla   !< Derivative of yield stress wrt equivalent plastic strain
        real(kind=WP), dimension(nel), intent(in)    :: normxx       !< 1st derivative of equivalent stress wrt stress xx
        real(kind=WP), dimension(nel), intent(in)    :: normyy       !< 1st derivative of equivalent stress wrt stress yy
        real(kind=WP), dimension(nel), intent(in)    :: normzz       !< 1st derivative of equivalent stress wrt stress zz
        real(kind=WP), dimension(nel), intent(in)    :: normxy       !< 1st derivative of equivalent stress wrt stress xy
        real(kind=WP), dimension(nel), intent(in)    :: normyz       !< 1st derivative of equivalent stress wrt stress yz
        real(kind=WP), dimension(nel), intent(in)    :: normzx       !< 1st derivative of equivalent stress wrt stress zx
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        real(kind=WP), dimension(nel) :: common_factor
!===============================================================================
!
        !=======================================================================
        !< - Prager kinematic hardening model
        !=======================================================================
        !< Compute the backstress components derivative w.r.t plastic multiplier
        common_factor(1:nel) = two_third * chard * dsigy_dpla(1:nel)
        dsigb_dlam(1:nel,1) = common_factor(1:nel) * normxx(1:nel)
        dsigb_dlam(1:nel,2) = common_factor(1:nel) * normyy(1:nel)
        dsigb_dlam(1:nel,3) = common_factor(1:nel) * normzz(1:nel)
        dsigb_dlam(1:nel,4) = common_factor(1:nel) * normxy(1:nel)
        dsigb_dlam(1:nel,5) = common_factor(1:nel) * normyz(1:nel)
        dsigb_dlam(1:nel,6) = common_factor(1:nel) * normzx(1:nel)
!
      end subroutine kinematic_hardening_prager
      end module kinematic_hardening_prager_mod
