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
!||    yield_criterion_barlat1989_mod          ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat1989.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress                ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_second_order_numerical   ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||====================================================================
      module yield_criterion_barlat1989_mod
! \brief Compute Barlat 1989 yield criterion for /MAT/LAW131
! \details Compute the equivalent stress and its first-order
!          derivatives using the Barlat 1989 anisotropic yield criterion
!          for /MAT/LAW131.
      contains
!||====================================================================
!||    yield_criterion_barlat1989              ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat1989.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress                ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_second_order_numerical   ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                            ../common_source/modules/constant_mod.F
!||    matparam_def_mod                        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod                               ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod                           ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine yield_criterion_barlat1989(                                   &
          matparam ,nel      ,seq      ,signxx   ,signyy   ,signxy   ,         &
          normxx   ,normyy   ,normzz   ,normxy   ,normyz   ,normzx   ,         &
          offset   )
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
        type(matparam_struct_),        intent(in)    :: matparam !< Material parameters data
        integer,                       intent(in)    :: nel      !< Number of elements in the group
        real(kind=WP), dimension(nel), intent(inout) :: seq      !< Equivalent stress
        real(kind=WP), dimension(nel), intent(in)    :: signxx   !< Current stress xx
        real(kind=WP), dimension(nel), intent(in)    :: signyy   !< Current stress yy
        real(kind=WP), dimension(nel), intent(in)    :: signxy   !< Current stress xy
        real(kind=WP), dimension(nel), intent(inout) :: normxx   !< 1st derivative of equivalent stress wrt stress xx
        real(kind=WP), dimension(nel), intent(inout) :: normyy   !< 1st derivative of equivalent stress wrt stress yy
        real(kind=WP), dimension(nel), intent(inout) :: normzz   !< 1st derivative of equivalent stress wrt stress zz
        real(kind=WP), dimension(nel), intent(inout) :: normxy   !< 1st derivative of equivalent stress wrt stress xy
        real(kind=WP), dimension(nel), intent(inout) :: normyz   !< 1st derivative of equivalent stress wrt stress yz
        real(kind=WP), dimension(nel), intent(inout) :: normzx   !< 1st derivative of equivalent stress wrt stress zx
        integer,                       intent(in)    :: offset   !< Offset in the material parameters array for yield criterion parameters
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i
        real(kind=WP) :: a,c,h,m,p
        real(kind=WP),dimension(nel) :: normsig,k1,k2
        real(kind=WP) :: dseq_dk1,dseq_dk2,dk1_dsigxx,dk1_dsigyy,dk2_dsigxx,   &
          dk2_dsigyy,dk2_dsigxy
!===============================================================================
!
        !=======================================================================
        !< - Barlat (1989) yield criterion and its derivatives
        !=======================================================================
        !< Barlat 1989 coefficients
        c = matparam%uparam(offset + 1)
        a = matparam%uparam(offset + 2)
        h = matparam%uparam(offset + 3)
        p = matparam%uparam(offset + 4)
        m = matparam%uparam(offset + 5)
        !< Shell element
        do i = 1,nel
          !< Norm of the stress tensor
          normsig(i) = signxx(i)*signxx(i)                                     &
                     + signyy(i)*signyy(i)                                     &
                 + two*signxy(i)*signxy(i)
          normsig(i) = sqrt(normsig(i))
          normsig(i) =  max(normsig(i),one)
!
          !< Equivalent stress
          k1(i) = half*(signxx(i) + h*signyy(i))/normsig(i)
          k2(i) = sqrt((half*(signxx(i)-h*signyy(i)))**2 +(p*signxy(i))**2)/   &
            normsig(i)
          seq(i) = a*(abs(k1(i)+k2(i)))**m +                                   &
            a*(abs(k1(i)-k2(i)))**m +                                          &
            c*(abs(two*k2(i)))**m
          if (seq(i) > zero) then
            seq(i) = exp((one/m)*log(half*seq(i)))
!
            !< Unnormalized equivalent stress
            seq(i) = seq(i)*normsig(i)
!
            !< Derivative of equivalent stress w.r.t k1 and k2
            dseq_dk1 = ((seq(i)/normsig(i))**(one-m))*(a/two)*(                  &
                     sign(one,k1(i) + k2(i))*(abs(k1(i) + k2(i)))**(m-one)       &
                   + sign(one,k1(i) - k2(i))*(abs(k1(i) - k2(i)))**(m-one))
            dseq_dk2 = ((seq(i)/normsig(i))**(one-m))*((a/two)*(                 &
                     sign(one,k1(i) + k2(i))*(abs(k1(i) + k2(i)))**(m-one)       &
                   - sign(one,k1(i) - k2(i))*(abs(k1(i) - k2(i)))**(m-one))      &
                   + c*(abs(two*k2(i)))**(m-one))
!
            !< Derivative of k1 w.r.t stress tensor components
            dk1_dsigxx = half
            dk1_dsigyy = h/two
!
            !< Derivative of k2 w.r.t stress tensor components
            dk2_dsigxx = (signxx(i)-h*signyy(i))/                               &
                         (max(normsig(i)*four*k2(i),em20))
            dk2_dsigyy = -h*(signxx(i)-h*signyy(i))/                            &
                         (max(normsig(i)*four*k2(i),em20))
            dk2_dsigxy = (p**2)*signxy(i)/max(normsig(i)*k2(i),em20)
!
            !< Assembling the derivative of the eq. stress w.r.t stress tensor
            normxx(i) = dseq_dk1*dk1_dsigxx + dseq_dk2*dk2_dsigxx
            normyy(i) = dseq_dk1*dk1_dsigyy + dseq_dk2*dk2_dsigyy
            normzz(i) = -(normxx(i) + normyy(i))
            normxy(i) = dseq_dk2*dk2_dsigxy       
            normyz(i) = zero
            normzx(i) = zero
!
          else
            seq(i)    = zero
            normxx(i) = zero
            normyy(i) = zero
            normzz(i) = zero
            normxy(i) = zero
            normyz(i) = zero
            normzx(i) = zero
          end if
        enddo
!
      end subroutine yield_criterion_barlat1989
      end module yield_criterion_barlat1989_mod
