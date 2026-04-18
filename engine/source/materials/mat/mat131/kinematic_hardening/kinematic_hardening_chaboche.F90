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
!||    kinematic_hardening_chaboche_mod     ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_chaboche.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_kinematic_hardening   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||====================================================================
      module kinematic_hardening_chaboche_mod
! \brief Compute Chaboche kinematic hardening for /MAT/LAW131
! \details Compute the Chaboche nonlinear kinematic hardening model
!          (backstress evolution) for /MAT/LAW131.
      contains
!||====================================================================
!||    kinematic_hardening_chaboche         ../engine/source/materials/mat/mat131/kinematic_hardening/kinematic_hardening_chaboche.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_kinematic_hardening   ../engine/source/materials/mat/mat131/elasto_plastic_kinematic_hardening.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                         ../common_source/modules/constant_mod.F
!||    matparam_def_mod                     ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    precision_mod                        ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine kinematic_hardening_chaboche(                                 &
        matparam ,nel      ,l_sigb   ,dsigb_dlam,sigb      ,chard    ,         &
        normxx   ,normyy   ,normzz   ,normxy    ,normyz    ,normzx   ,         &
        dpla_dlam,offset   )
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
        type(matparam_struct_),        intent(in)    :: matparam     !< Material parameters data
        integer,                       intent(in)    :: nel          !< Number of elements in the group
        integer,                       intent(in)    :: l_sigb       !< Number of backstress components
        real(kind=WP), dimension(nel,l_sigb),intent(inout) :: dsigb_dlam !< Backstress components derivative w.r.t plastic multiplier
        real(kind=WP),                 intent(in)    :: chard        !< Mixed hardening parameter
        real(kind=WP), dimension(nel,l_sigb),intent(in) :: sigb      !< Backstress components for kinematic hardening
        real(kind=WP), dimension(nel), intent(in)    :: normxx       !< 1st derivative of equivalent stress wrt stress xx
        real(kind=WP), dimension(nel), intent(in)    :: normyy       !< 1st derivative of equivalent stress wrt stress yy
        real(kind=WP), dimension(nel), intent(in)    :: normzz       !< 1st derivative of equivalent stress wrt stress zz
        real(kind=WP), dimension(nel), intent(in)    :: normxy       !< 1st derivative of equivalent stress wrt stress xy
        real(kind=WP), dimension(nel), intent(in)    :: normyz       !< 1st derivative of equivalent stress wrt stress yz
        real(kind=WP), dimension(nel), intent(in)    :: normzx       !< 1st derivative of equivalent stress wrt stress zx
        real(kind=WP), dimension(nel), intent(in)    :: dpla_dlam    !< Derivative of equivalent plastic strain w.r.t plastic multiplier
        integer,                       intent(in)    :: offset       !< Offset in the material parameters array for kinematic hardening parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: k
        real(kind=WP) :: akh(4),ckh(4),factor1,factor2
!===============================================================================
!
        !=======================================================================
        !< - Chaboche-Rousselier kinematic hardening model
        !=======================================================================
        !< Recover kinematic hardening parameters
        ckh(1) = matparam%uparam(offset + 1) !< Chaboche kinematic hardening parameter for the 1st backstress component
        akh(1) = matparam%uparam(offset + 2) !< Chaboche kinematic hardening parameter for the 1st backstress component
        ckh(2) = matparam%uparam(offset + 3) !< Chaboche kinematic hardening parameter for the 2nd backstress component
        akh(2) = matparam%uparam(offset + 4) !< Chaboche kinematic hardening parameter for the 2nd backstress component
        ckh(3) = matparam%uparam(offset + 5) !< Chaboche kinematic hardening parameter for the 3rd backstress component
        akh(3) = matparam%uparam(offset + 6) !< Chaboche kinematic hardening parameter for the 3rd backstress component
        ckh(4) = matparam%uparam(offset + 7) !< Chaboche kinematic hardening parameter for the 4th backstress component
        akh(4) = matparam%uparam(offset + 8) !< Chaboche kinematic hardening parameter for the 4th backstress component
        !< Compute the backstress components derivative w.r.t plastic multiplier
        do k = 1, 4
          factor1 = chard * akh(k) * ckh(k)
          factor2 = chard * ckh(k)
          select case (k)
          case (1)
            dsigb_dlam(1:nel, 1) = factor1*normxx(1:nel) - factor2*sigb(1:nel,1)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel, 2) = factor1*normyy(1:nel) - factor2*sigb(1:nel,2)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel, 3) = factor1*normzz(1:nel) - factor2*sigb(1:nel,3)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel, 4) = factor1*normxy(1:nel) - factor2*sigb(1:nel,4)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel, 5) = factor1*normyz(1:nel) - factor2*sigb(1:nel,5)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel, 6) = factor1*normzx(1:nel) - factor2*sigb(1:nel,6)*dpla_dlam(1:nel)
          case (2)
            dsigb_dlam(1:nel, 7) = factor1*normxx(1:nel) - factor2*sigb(1:nel,7) *dpla_dlam(1:nel)
            dsigb_dlam(1:nel, 8) = factor1*normyy(1:nel) - factor2*sigb(1:nel,8) *dpla_dlam(1:nel)
            dsigb_dlam(1:nel, 9) = factor1*normzz(1:nel) - factor2*sigb(1:nel,9) *dpla_dlam(1:nel)
            dsigb_dlam(1:nel,10) = factor1*normxy(1:nel) - factor2*sigb(1:nel,10)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,11) = factor1*normyz(1:nel) - factor2*sigb(1:nel,11)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,12) = factor1*normzx(1:nel) - factor2*sigb(1:nel,12)*dpla_dlam(1:nel)
          case (3)
            dsigb_dlam(1:nel,13) = factor1*normxx(1:nel) - factor2*sigb(1:nel,13)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,14) = factor1*normyy(1:nel) - factor2*sigb(1:nel,14)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,15) = factor1*normzz(1:nel) - factor2*sigb(1:nel,15)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,16) = factor1*normxy(1:nel) - factor2*sigb(1:nel,16)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,17) = factor1*normyz(1:nel) - factor2*sigb(1:nel,17)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,18) = factor1*normzx(1:nel) - factor2*sigb(1:nel,18)*dpla_dlam(1:nel)
          case (4)
            dsigb_dlam(1:nel,19) = factor1*normxx(1:nel) - factor2*sigb(1:nel,19)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,20) = factor1*normyy(1:nel) - factor2*sigb(1:nel,20)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,21) = factor1*normzz(1:nel) - factor2*sigb(1:nel,21)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,22) = factor1*normxy(1:nel) - factor2*sigb(1:nel,22)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,23) = factor1*normyz(1:nel) - factor2*sigb(1:nel,23)*dpla_dlam(1:nel)
            dsigb_dlam(1:nel,24) = factor1*normzx(1:nel) - factor2*sigb(1:nel,24)*dpla_dlam(1:nel)
          end select
        enddo
!
      end subroutine kinematic_hardening_chaboche
      end module kinematic_hardening_chaboche_mod
