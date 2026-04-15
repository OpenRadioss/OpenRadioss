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
!||    yield_criterion_barlat2000_mod          ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat2000.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress                ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_second_order_numerical   ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||====================================================================
      module yield_criterion_barlat2000_mod
! \brief Compute Barlat 2000 yield criterion for /MAT/LAW131
! \details Compute the equivalent stress and its first-order
!          derivatives using the Barlat 2000 (Yld2000-2d) anisotropic
!          yield criterion for /MAT/LAW131.
      contains
!||====================================================================
!||    yield_criterion_barlat2000              ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_barlat2000.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress                ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||    elasto_plastic_second_order_numerical   ../engine/source/materials/mat/mat131/elasto_plastic_second_order_numerical.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod                            ../common_source/modules/constant_mod.F
!||    matparam_def_mod                        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod                               ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod                           ../common_source/modules/precision_mod.F90
!||    yield_criterion_vonmises_mod            ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_vonmises.F90
!||====================================================================
        subroutine yield_criterion_barlat2000(                                   &
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
        real(kind=WP) :: al1,al2,al3,al4,al5,al6,al7,al8,expa
        real(kind=WP) :: lp11,lp12,lp21,lp22,lp66
        real(kind=WP) :: lpp11,lpp12,lpp21,lpp22,lpp66
        real(kind=WP) :: xpxx(nel),xpyy(nel),xpxy(nel)
        real(kind=WP) :: xppxx(nel),xppyy(nel),xppxy(nel)
        real(kind=WP),dimension(nel) :: normsig,xp1,xp2,xpp1,xpp2,phip,phipp
        real(kind=WP) :: mohr_center,mohr_radius,dxp1dxpxx,dxp1dxpyy,dxp1dxpxy,&
          dxp2dxpxx,dxp2dxpyy,dxp2dxpxy,dphipdsigxx,dphipdsigyy,dphipdsigxy,   &
          dxpp1dxppxx,dxpp1dxppyy,dxpp1dxppxy,dxpp2dxppxx,dxpp2dxppyy,         &
          dxpp2dxppxy,dphippdsigxx,dphippdsigyy,dphippdsigxy,                  &
          dphipdxp1,dphipdxp2,dphippdxpp1,dphippdxpp2,dseqdphip,dseqdphipp,    &
          dxp1dsigxx,dxp1dsigyy,dxp1dsigxy,dxp2dsigxx,dxp2dsigyy,dxp2dsigxy,   &
          dxpp1dsigxx,dxpp1dsigyy,dxpp1dsigxy,dxpp2dsigxx,dxpp2dsigyy,         &
          dxpp2dsigxy
!===============================================================================
!
        !=======================================================================
        !< - Barlat (2000) yield criterion and its derivatives
        !=======================================================================
        !< Barlat 2000 coefficients
        lp11  = matparam%uparam(offset + 1)
        lp12  = matparam%uparam(offset + 2)
        lp21  = matparam%uparam(offset + 3)
        lp22  = matparam%uparam(offset + 4)
        lp66  = matparam%uparam(offset + 5)
        lpp11 = matparam%uparam(offset + 6)
        lpp12 = matparam%uparam(offset + 7)
        lpp21 = matparam%uparam(offset + 8)
        lpp22 = matparam%uparam(offset + 9)
        lpp66 = matparam%uparam(offset + 10)
        expa  = matparam%uparam(offset + 11)
!
          !< Shell element
          do i = 1,nel
            !< Norm of the stress tensor
            normsig(i) = signxx(i)*signxx(i)                                     &
              + signyy(i)*signyy(i)                                     &
              + two*signxy(i)*signxy(i)
            normsig(i) = sqrt(normsig(i))
            normsig(i) =  max(normsig(i),one)
!
            !< Computation of the xprime and xprimeprime tensors
            xpxx(i)  = (lp11*signxx(i) + lp12*signyy(i))/normsig(i)
            xpyy(i)  = (lp21*signxx(i) + lp22*signyy(i))/normsig(i)
            xpxy(i)  =  lp66*signxy(i)/normsig(i)
!
            xppxx(i) = (lpp11*signxx(i) + lpp12*signyy(i))/normsig(i)
            xppyy(i) = (lpp21*signxx(i) + lpp22*signyy(i))/normsig(i)
            xppxy(i) =  lpp66*signxy(i)/normsig(i)
!
            !< Computation of the xprime and xprimeprime principal stresses
            mohr_center = (xpxx(i)+xpyy(i))/two
            mohr_radius = sqrt(((xpxx(i)-xpyy(i))/two)**2 + xpxy(i)**2)
            mohr_radius = max(em20,mohr_radius)
            xp1(i) = mohr_center + mohr_radius
            xp2(i) = mohr_center - mohr_radius
            !< Derivative of xprime 1 w.r.t xprime tensor
            dxp1dxpxx = half*(one + (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp1dxpyy = half*(one - (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp1dxpxy = xpxy(i)/mohr_radius
            !< Derivative of xprime 2 w.r.t xprime tensor
            dxp2dxpxx = half*(one - (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp2dxpyy = half*(one + (xpxx(i)-xpyy(i))/(two*mohr_radius))
            dxp2dxpxy = -xpxy(i)/mohr_radius
!
            !< Computation of xprimeprime principal stresses
            mohr_center = (xppxx(i)+xppyy(i))/two
            mohr_radius = sqrt(((xppxx(i)-xppyy(i))/two)**2 + xppxy(i)**2)
            mohr_radius = max(em20,mohr_radius)
            xpp1(i) = mohr_center + mohr_radius
            xpp2(i) = mohr_center - mohr_radius
            !< Derivative of xprimeprime 1 w.r.t xprimeprime tensor
            dxpp1dxppxx = half*(one + (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp1dxppyy = half*(one - (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp1dxppxy = xppxy(i)/mohr_radius
            !< Derivative of xprimeprime 2 w.r.t xprimeprime tensor
            dxpp2dxppxx = half*(one - (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp2dxppyy = half*(one + (xppxx(i)-xppyy(i))/(two*mohr_radius))
            dxpp2dxppxy = -xppxy(i)/mohr_radius
!
            !< Computation of the phiprime and phiprimeprime functions
            phip(i)  = (abs(xp1(i) - xp2(i)))**expa
            phipp(i) = (abs(two*xpp2(i) + xpp1(i)))**expa +                      &
              (abs(two*xpp1(i) + xpp2(i)))**expa
!
            !< Equivalent stress
            seq(i) = half*(phip(i)+phipp(i))
            if (seq(i) > zero) then
              seq(i) = exp((one/expa)*log(seq(i)))
            else
              seq(i) = zero
            end if
            seq(i) = seq(i)*normsig(i)
!
            !< Assembling derivative of xprime 1 w.r.t stress tensor
            dxp1dsigxx = dxp1dxpxx*lp11 + dxp1dxpyy*lp21
            dxp1dsigyy = dxp1dxpxx*lp12 + dxp1dxpyy*lp22
            dxp1dsigxy = dxp1dxpxy*lp66
!
            !< Assembling derivative of xprime 2 w.r.t stress tensor
            dxp2dsigxx = dxp2dxpxx*lp11 + dxp2dxpyy*lp21
            dxp2dsigyy = dxp2dxpxx*lp12 + dxp2dxpyy*lp22
            dxp2dsigxy = dxp2dxpxy*lp66
!
            !< Assembling derivative of xprimeprime 1 w.r.t stress tensor
            dxpp1dsigxx = dxpp1dxppxx*lpp11 + dxpp1dxppyy*lpp21
            dxpp1dsigyy = dxpp1dxppxx*lpp12 + dxpp1dxppyy*lpp22
            dxpp1dsigxy = dxpp1dxppxy*lpp66
!
            !< Assembling derivative of xprimeprime 2 w.r.t stress tensor
            dxpp2dsigxx = dxpp2dxppxx*lpp11 + dxpp2dxppyy*lpp21
            dxpp2dsigyy = dxpp2dxppxx*lpp12 + dxpp2dxppyy*lpp22
            dxpp2dsigxy = dxpp2dxppxy*lpp66
!
            !< Derivative of phiprime w.r.t xprime 1
            dphipdxp1 = expa*(abs(xp1(i)-xp2(i)))**(expa-1)*sign(one,xp1(i)-xp2(i))
            !< Derivative of phiprime w.r.t xprime 2
            dphipdxp2 = -dphipdxp1
!
            !< Derivative of phiprimeprime w.r.t xprimeprime 1
            dphippdxpp1 = expa*(abs(two*xpp2(i)+xpp1(i)))**(expa-1)*             &
              sign(one,two*xpp2(i)+xpp1(i)) +                                    &
              two*expa*(abs(two*xpp1(i)+xpp2(i)))**(expa-1)*                     &
              sign(one,two*xpp1(i)+xpp2(i))
            !< Derivative of phiprimeprime w.r.t xprimeprime 2
            dphippdxpp2 = expa*(abs(two*xpp1(i)+xpp2(i)))**(expa-1)*             &
              sign(one,two*xpp1(i)+xpp2(i)) +                                    &
              two*expa*(abs(two*xpp2(i)+xpp1(i)))**(expa-1)*                     &
              sign(one,two*xpp2(i)+xpp1(i))
!
            !< Assembling derivative of phiprime w.r.t stress tensor
            dphipdsigxx = dphipdxp1*dxp1dsigxx + dphipdxp2*dxp2dsigxx
            dphipdsigyy = dphipdxp1*dxp1dsigyy + dphipdxp2*dxp2dsigyy
            dphipdsigxy = dphipdxp1*dxp1dsigxy + dphipdxp2*dxp2dsigxy
!
            !< Assembling derivative of phiprimeprime w.r.t stress tensor
            dphippdsigxx = dphippdxpp1*dxpp1dsigxx + dphippdxpp2*dxpp2dsigxx
            dphippdsigyy = dphippdxpp1*dxpp1dsigyy + dphippdxpp2*dxpp2dsigyy
            dphippdsigxy = dphippdxpp1*dxpp1dsigxy + dphippdxpp2*dxpp2dsigxy
!
            !< Derivative of equivalent stress w.r.t phiprime
            dseqdphip  =                                                         &
              (half/expa)*exp((one/expa - one)*log(half*(phip(i)+phipp(i))))
            !< Derivative of equivalent stress w.r.t phiprimeprime
            dseqdphipp = dseqdphip
!
            !< Assembling derivative of equivalent stress w.r.t stress tensor
            normxx(i) = dseqdphip*dphipdsigxx + dseqdphipp*dphippdsigxx
            normyy(i) = dseqdphip*dphipdsigyy + dseqdphipp*dphippdsigyy
            normzz(i) = - (normxx(i) + normyy(i))
            normxy(i) = dseqdphip*dphipdsigxy + dseqdphipp*dphippdsigxy
            normyz(i) = zero
            normzx(i) = zero
!
          enddo
!
        end subroutine yield_criterion_barlat2000
      end module yield_criterion_barlat2000_mod
