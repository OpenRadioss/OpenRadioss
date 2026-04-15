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
!||    yield_criterion_hill_mod   ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hill.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress   ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||====================================================================
      module yield_criterion_hill_mod
! \brief Compute Hill yield criterion for /MAT/LAW131
! \details Compute the equivalent stress and its first and second-order
!          derivatives using the Hill 1948 anisotropic yield criterion
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    yield_criterion_hill           ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hill.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress       ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                   ../common_source/modules/constant_mod.F
!||    matparam_def_mod               ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod                      ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod                  ../common_source/modules/precision_mod.F90
!||    yield_criterion_vonmises_mod   ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_vonmises.F90
!||====================================================================
      subroutine yield_criterion_hill(                                         &
          matparam ,nel      ,seq      ,eltype   ,                             &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,         &
          N2       ,second_order,offset)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use mvsiz_mod
        use precision_mod, only : WP
        use yield_criterion_vonmises_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
        integer,                       intent(in)    :: nel      !< Number of elements in the group
        real(kind=WP), dimension(nel), intent(inout) :: seq      !< Equivalent stress
        integer,                       intent(in)    :: eltype   !< Element type
        real(kind=WP), dimension(nel), intent(in)    :: signxx   !< Current stress xx
        real(kind=WP), dimension(nel), intent(in)    :: signyy   !< Current stress yy
        real(kind=WP), dimension(nel), intent(in)    :: signzz   !< Current stress zz
        real(kind=WP), dimension(nel), intent(in)    :: signxy   !< Current stress xy
        real(kind=WP), dimension(nel), intent(in)    :: signyz   !< Current stress yz
        real(kind=WP), dimension(nel), intent(in)    :: signzx   !< Current stress zx
        real(kind=WP), dimension(nel), intent(inout) :: normxx   !< 1st derivative of equivalent stress wrt stress xx
        real(kind=WP), dimension(nel), intent(inout) :: normyy   !< 1st derivative of equivalent stress wrt stress yy
        real(kind=WP), dimension(nel), intent(inout) :: normzz   !< 1st derivative of equivalent stress wrt stress zz
        real(kind=WP), dimension(nel), intent(inout) :: normxy   !< 1st derivative of equivalent stress wrt stress xy
        real(kind=WP), dimension(nel), intent(inout) :: normyz   !< 1st derivative of equivalent stress wrt stress yz
        real(kind=WP), dimension(nel), intent(inout) :: normzx   !< 1st derivative of equivalent stress wrt stress zx
        real(kind=WP), dimension(nel,6,6), intent(inout) :: N2   !< 2nd derivative of equivalent stress
        logical,                       intent(in)    :: second_order !< Flag for computing second order derivatives
        integer,                       intent(in)    :: offset   !< Offset in the material parameters array for yield criterion parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        real(kind=WP) :: F,G,H,L,M,N
        real(kind=WP), dimension(nel) :: s_yy_minus_s_zz, s_zz_minus_s_xx,     &
          s_xx_minus_s_yy,inv_seq
!===============================================================================
!
        !=======================================================================
        !< - Hill (1948) yield criterion and its derivatives
        !=======================================================================
        !< Hill coefficients
        F = matparam%uparam(offset + 1)
        G = matparam%uparam(offset + 2)
        H = matparam%uparam(offset + 3)
        L = matparam%uparam(offset + 4)
        M = matparam%uparam(offset + 5)
        N = matparam%uparam(offset + 6)
        !< Solid element
        if (eltype == 1) then
          s_yy_minus_s_zz(1:nel) = signyy(1:nel) - signzz(1:nel)
          s_zz_minus_s_xx(1:nel) = signzz(1:nel) - signxx(1:nel)
          s_xx_minus_s_yy(1:nel) = signxx(1:nel) - signyy(1:nel)
          !< Equivalent stress
          seq(1:nel) = F*s_yy_minus_s_zz(1:nel)**two + G*s_zz_minus_s_xx(1:nel)**two +&
                       H*s_xx_minus_s_yy(1:nel)**two + L*two*signyz(1:nel)**two +     &
                       M*two*signzx(1:nel)**two + N*two*signxy(1:nel)**two                          
          seq(1:nel) = sqrt(seq(1:nel))
          !< First order derivative of eq. stress
          inv_seq(1:nel) = one / max(seq(1:nel), em20)
          normxx(1:nel) = inv_seq(1:nel)*( H*s_xx_minus_s_yy(1:nel) - G*s_zz_minus_s_xx(1:nel)) 
          normyy(1:nel) = inv_seq(1:nel)*( F*s_yy_minus_s_zz(1:nel) - H*s_xx_minus_s_yy(1:nel))
          normzz(1:nel) = inv_seq(1:nel)*( G*s_zz_minus_s_xx(1:nel) - F*s_yy_minus_s_zz(1:nel))
          normxy(1:nel) = inv_seq(1:nel) * N*two*signxy(1:nel)
          normyz(1:nel) = inv_seq(1:nel) * L*two*signyz(1:nel)
          normzx(1:nel) = inv_seq(1:nel) * M*two*signzx(1:nel)
          !< Second order derivative of eq. stress
          if (second_order) then 
            N2(1:nel,1:6,1:6) = zero
            N2(1:nel,1,1) = inv_seq(1:nel) * ((H+G) - normxx(1:nel)**two) 
            N2(1:nel,1,2) = inv_seq(1:nel) * (- H - normyy(1:nel)*normxx(1:nel)) 
            N2(1:nel,1,3) = inv_seq(1:nel) * (- G - normzz(1:nel)*normxx(1:nel))
            N2(1:nel,1,4) = inv_seq(1:nel) * (    - normxy(1:nel)*normxx(1:nel))
            N2(1:nel,1,5) = inv_seq(1:nel) * (    - normyz(1:nel)*normxx(1:nel))
            N2(1:nel,1,6) = inv_seq(1:nel) * (    - normzx(1:nel)*normxx(1:nel))
            N2(1:nel,2,1) = N2(1:nel,1,2)
            N2(1:nel,2,2) = inv_seq(1:nel) * ((F+H) - normyy(1:nel)**two)
            N2(1:nel,2,3) = inv_seq(1:nel) * (- F - normzz(1:nel)*normyy(1:nel))
            N2(1:nel,2,4) = inv_seq(1:nel) * (    - normxy(1:nel)*normyy(1:nel))
            N2(1:nel,2,5) = inv_seq(1:nel) * (    - normyz(1:nel)*normyy(1:nel))
            N2(1:nel,2,6) = inv_seq(1:nel) * (    - normzx(1:nel)*normyy(1:nel))
            N2(1:nel,3,1) = N2(1:nel,1,3)
            N2(1:nel,3,2) = N2(1:nel,2,3)
            N2(1:nel,3,3) = inv_seq(1:nel) * ((G+F) - normzz(1:nel)**two)
            N2(1:nel,3,4) = inv_seq(1:nel) * (   - normxy(1:nel)*normzz(1:nel))
            N2(1:nel,3,5) = inv_seq(1:nel) * (   - normyz(1:nel)*normzz(1:nel))
            N2(1:nel,3,6) = inv_seq(1:nel) * (   - normzx(1:nel)*normzz(1:nel))
            N2(1:nel,4,1) = N2(1:nel,1,4)
            N2(1:nel,4,2) = N2(1:nel,2,4)
            N2(1:nel,4,3) = N2(1:nel,3,4)
            N2(1:nel,4,4) = inv_seq(1:nel) * (N*two - normxy(1:nel)**two)
            N2(1:nel,4,5) = inv_seq(1:nel) * (   - normyz(1:nel)*normxy(1:nel))
            N2(1:nel,4,6) = inv_seq(1:nel) * (   - normzx(1:nel)*normxy(1:nel))
            N2(1:nel,5,1) = N2(1:nel,1,5)
            N2(1:nel,5,2) = N2(1:nel,2,5)
            N2(1:nel,5,3) = N2(1:nel,3,5)
            N2(1:nel,5,4) = N2(1:nel,4,5)
            N2(1:nel,5,5) = inv_seq(1:nel) * (L*two - normyz(1:nel)**two)
            N2(1:nel,5,6) = inv_seq(1:nel) * (   - normzx(1:nel)*normyz(1:nel))
            N2(1:nel,6,1) = N2(1:nel,1,6)
            N2(1:nel,6,2) = N2(1:nel,2,6)
            N2(1:nel,6,3) = N2(1:nel,3,6)
            N2(1:nel,6,4) = N2(1:nel,4,6)
            N2(1:nel,6,5) = N2(1:nel,5,6)
            N2(1:nel,6,6) = inv_seq(1:nel) * (M*two - normzx(1:nel)**two)
          endif 
        !< Shell element
        elseif (eltype == 2) then
          s_xx_minus_s_yy(1:nel) = signxx(1:nel) - signyy(1:nel)
          seq(1:nel) = F*signyy(1:nel)**two + G*signxx(1:nel)**two +           &
                       H*s_xx_minus_s_yy(1:nel)**two + N*two*signxy(1:nel)**two
          seq(1:nel) = sqrt(seq(1:nel))
          !< First order derivative of eq. stress
          inv_seq(1:nel) = one / max(seq(1:nel), em20)
          normxx(1:nel) = inv_seq(1:nel)*( H*s_xx_minus_s_yy(1:nel) + G*signxx(1:nel))
          normyy(1:nel) = inv_seq(1:nel)*(-H*s_xx_minus_s_yy(1:nel) + F*signyy(1:nel))
          normzz(1:nel) = - normxx(1:nel) - normyy(1:nel)
          normxy(1:nel) = inv_seq(1:nel)*N*two*signxy(1:nel)     
          normyz(1:nel) = zero
          normzx(1:nel) = zero
          if (second_order) then 
            N2(1:nel,1:6,1:6) = zero
            N2(1:nel,1,1) = inv_seq(1:nel) * ((H+G) - normxx(1:nel)**two) 
            N2(1:nel,1,2) = inv_seq(1:nel) * (- H - normyy(1:nel)*normxx(1:nel)) 
            N2(1:nel,1,4) = inv_seq(1:nel) * (    - normxy(1:nel)*normxx(1:nel))
            N2(1:nel,2,1) = N2(1:nel,1,2)
            N2(1:nel,2,2) = inv_seq(1:nel) * ((F+H) - normyy(1:nel)**two)
            N2(1:nel,2,4) = inv_seq(1:nel) * (    - normxy(1:nel)*normyy(1:nel))
            N2(1:nel,4,1) = N2(1:nel,1,4)
            N2(1:nel,4,2) = N2(1:nel,2,4)
            N2(1:nel,4,4) = inv_seq(1:nel) * (N*two - normxy(1:nel)**two)
          endif 
        endif
!
      end subroutine yield_criterion_hill
      end module yield_criterion_hill_mod
