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
!||    yield_criterion_vonmises_mod   ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_vonmises.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress       ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    yield_criterion_barlat2000     ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat2000.F90
!||    yield_criterion_hershey        ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hershey.F90
!||    yield_criterion_hill           ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_hill.F90
!||====================================================================
      module yield_criterion_vonmises_mod
! \brief Compute von Mises yield criterion for /MAT/LAW131
! \details Compute the equivalent stress and its first and second-order
!          derivatives using the von Mises isotropic yield criterion
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    yield_criterion_vonmises   ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_vonmises.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress   ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod               ../common_source/modules/constant_mod.F
!||    matparam_def_mod           ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod                  ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine yield_criterion_vonmises(                                     &
          nel      ,seq      ,eltype   ,                                       &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   ,         &
          normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,         &
          N        ,second_order)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use mvsiz_mod
        use precision_mod, only : WP
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
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
        real(kind=WP), dimension(nel,6,6), intent(inout) :: N    !< 2nd derivative of equivalent stress
        logical,                       intent(in)    :: second_order !< Flag for computing second order derivatives
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        real(kind=WP), dimension(nel) :: inv_seq
!===============================================================================
!
        !=======================================================================
        !< - Von Mises yield criterion and its derivatives
        !=======================================================================
        !< Solid element
        if (eltype == 1) then 
          !< Equivalent stress
          seq(1:nel) =  half*(signyy(1:nel)-signzz(1:nel))**2 +                &                 
                        half*(signzz(1:nel)-signxx(1:nel))**2 +                & 
                        half*(signxx(1:nel)-signyy(1:nel))**2 +                &
                       three*(signxy(1:nel))**2               +                &
                       three*(signyz(1:nel))**2               +                &
                       three*(signzx(1:nel))**2 
          seq(1:nel) = sqrt(seq(1:nel))
          !< First order derivative of eq. stress
          inv_seq(1:nel) = one / max(seq(1:nel), em20)
          normxx(1:nel) = half*inv_seq(1:nel)*                                 &
                     (two*signxx(1:nel) - signyy(1:nel) - signzz(1:nel))
          normyy(1:nel) = half*inv_seq(1:nel)*                                 &
                     (two*signyy(1:nel) - signxx(1:nel) - signzz(1:nel))
          normzz(1:nel) = half*inv_seq(1:nel)*                                 &
                     (two*signzz(1:nel) - signyy(1:nel) - signxx(1:nel))
          normxy(1:nel) = inv_seq(1:nel)*three*signxy(1:nel)
          normyz(1:nel) = inv_seq(1:nel)*three*signyz(1:nel)
          normzx(1:nel) = inv_seq(1:nel)*three*signzx(1:nel)              
          !< Second order derivative of eq. stress
          if (second_order) then 
            N(1:nel,1:6,1:6) = zero
            N(1:nel,1,1) = inv_seq(1:nel)*(one - normxx(1:nel)**2)
            N(1:nel,1,2) = inv_seq(1:nel)*(- half - normyy(1:nel)*normxx(1:nel))
            N(1:nel,1,3) = inv_seq(1:nel)*(- half - normzz(1:nel)*normxx(1:nel))
            N(1:nel,2,1) = inv_seq(1:nel)*(- half - normxx(1:nel)*normyy(1:nel))
            N(1:nel,2,2) = inv_seq(1:nel)*(one - normyy(1:nel)**2)
            N(1:nel,2,3) = inv_seq(1:nel)*(- half - normzz(1:nel)*normyy(1:nel))
            N(1:nel,3,1) = inv_seq(1:nel)*(- half - normxx(1:nel)*normzz(1:nel))
            N(1:nel,3,2) = inv_seq(1:nel)*(- half - normyy(1:nel)*normzz(1:nel))
            N(1:nel,3,3) = inv_seq(1:nel)*(one - normzz(1:nel)**2)
            N(1:nel,4,4) = inv_seq(1:nel)*(three - normxy(1:nel)**2)
            N(1:nel,5,5) = inv_seq(1:nel)*(three - normyz(1:nel)**2)
            N(1:nel,6,6) = inv_seq(1:nel)*(three - normzx(1:nel)**2)              
          endif      
        !< Shell element
        elseif (eltype == 2) then 
          !< Equivalent stress
          seq(1:nel) = signxx(1:nel)**2 + signyy(1:nel)**2 -                   &
                       signxx(1:nel)*signyy(1:nel) + three*(signxy(1:nel)**2)
          seq(1:nel) = sqrt(seq(1:nel))
          !< First order derivative of eq. stress
          inv_seq(1:nel) = one / max(seq(1:nel), em20)
          normxx(1:nel) =  half*inv_seq(1:nel)*(two*signxx(1:nel) - signyy(1:nel))
          normyy(1:nel) =  half*inv_seq(1:nel)*(two*signyy(1:nel) - signxx(1:nel))
          normzz(1:nel) = - normxx(1:nel) - normyy(1:nel)
          normxy(1:nel) = three*inv_seq(1:nel)*signxy(1:nel)
          normyz(1:nel) = zero
          normzx(1:nel) = zero
          !< Second order derivative of eq. stress
          if (second_order) then 
            N(1:nel,1:6,1:6) = zero
            N(1:nel,1,1) = inv_seq(1:nel)*(one - normxx(1:nel)**2)
            N(1:nel,1,2) = inv_seq(1:nel)*(- half - normyy(1:nel)*normxx(1:nel))
            N(1:nel,2,1) = inv_seq(1:nel)*(- half - normxx(1:nel)*normyy(1:nel))
            N(1:nel,2,2) = inv_seq(1:nel)*(one - normyy(1:nel)**2)
            N(1:nel,4,4) = inv_seq(1:nel)*(three - normxy(1:nel)**2)             
          endif    
        endif
!
      end subroutine yield_criterion_vonmises
      end module yield_criterion_vonmises_mod
