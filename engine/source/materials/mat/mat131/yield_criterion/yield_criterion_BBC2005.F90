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
!||    yield_criterion_bbc2005_mod   ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_BBC2005.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress      ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||====================================================================
      module yield_criterion_bbc2005_mod
      contains
!||====================================================================
!||    yield_criterion_bbc2005    ../engine/source/materials/mat/mat131/yield_criterion/yield_criterion_BBC2005.F90
!||--- called by ------------------------------------------------------
!||    elasto_plastic_eq_stress   ../engine/source/materials/mat/mat131/elasto_plastic_eq_stress.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod               ../common_source/modules/constant_mod.F
!||    matparam_def_mod           ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod                  ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod              ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine yield_criterion_bbc2005(                                      &
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
        integer :: i, int_2k, int_2km1
        real(kind=WP) :: a, b_coeff, l_coeff, m_coeff, n_coeff, p, q, r, k_val
        real(kind=WP) :: gamma, lambda, psi, W, seq_inv
        real(kind=WP) :: dW_dgamma, dW_dlambda, dW_dpsi
        real(kind=WP) :: dlam_dxx, dlam_dyy, dlam_dxy
        real(kind=WP) :: dpsi_dxx, dpsi_dyy, dpsi_dxy
        real(kind=WP) :: term_NP, term_QR
        real(kind=WP) :: lam_p_gam, lam_m_gam, lam_p_psi, lam_m_psi
!===============================================================================
!
        !=======================================================================
        !< - BBC 2005 yield criterion and its derivatives
        !=======================================================================
        !< Retrieve BBC 2005 math coefficients from array
        a       = matparam%uparam(offset + 1)
        b_coeff = matparam%uparam(offset + 2)
        l_coeff = matparam%uparam(offset + 3)
        m_coeff = matparam%uparam(offset + 4)
        n_coeff = matparam%uparam(offset + 5)
        p       = matparam%uparam(offset + 6)
        q       = matparam%uparam(offset + 7)
        r       = matparam%uparam(offset + 8)
        k_val   = matparam%uparam(offset + 9)
        !< Calculate integer exponents for power laws
        int_2k = nint(two * k_val)
        int_2km1 = int_2k - 1

        !< Shell element loop (fully vectorized)
        do i = 1,nel
          !< 1. Calculate internal scalar variables Gamma, Lambda, Psi
          gamma   = l_coeff * signxx(i) + m_coeff * signyy(i) !OK
          term_NP = n_coeff * signxx(i) - p * signyy(i)
          term_QR = q * signxx(i) - r * signyy(i)

          lambda = sqrt(term_NP * term_NP + signxy(i) * signxy(i)) !OK
          psi    = sqrt(term_QR * term_QR + signxy(i) * signxy(i)) !OK

          lam_p_gam = lambda + gamma
          lam_m_gam = lambda - gamma
          lam_p_psi = lambda + psi
          lam_m_psi = lambda - psi

          !< 2. Compute Yield Function W and Equivalent Stress
          W = a * (lam_p_gam**int_2k + lam_m_gam**int_2k) +                    &
              b_coeff * (lam_p_psi**int_2k + lam_m_psi**int_2k) !OK
          
          if (W > zero) then
            !< Equivalent stress
            seq(i) = W**(one / real(int_2k)) !OK
            
            !< Pre-factor for derivatives: seq(i)^(1 - 2k)
            !< This eliminates the (1/2k) * (2k) constants from chain rule
            seq_inv = seq(i)**(one - real(int_2k)) !OK

            !< 3. Derivatives of W wrt Gamma, Lambda, Psi
            dW_dgamma  = a * (lam_p_gam**int_2km1 - lam_m_gam**int_2km1)
            dW_dlambda = a * (lam_p_gam**int_2km1 + lam_m_gam**int_2km1) +     &
                         b_coeff * (lam_p_psi**int_2km1 + lam_m_psi**int_2km1)
            dW_dpsi    = b_coeff * (lam_p_psi**int_2km1 - lam_m_psi**int_2km1)

            !< Scale by seq_inv
            dW_dgamma  = dW_dgamma * seq_inv !ok
            dW_dlambda = dW_dlambda * seq_inv !ok
            dW_dpsi    = dW_dpsi * seq_inv !ok

            !< 4. Derivatives of Lambda and Psi wrt stress tensor components
            !< Denominator protection (em20) prevents NaN at zero shear
            dlam_dxx = n_coeff * term_NP / max(lambda, em20) !OK
            dlam_dyy = -p * term_NP / max(lambda, em20) !OK
            dlam_dxy = signxy(i) / max(lambda, em20) !OK

            dpsi_dxx = q * term_QR / max(psi, em20) !OK
            dpsi_dyy = -r * term_QR / max(psi, em20) !OK
            dpsi_dxy = signxy(i) / max(psi, em20) !OK

            !< 5. Assembling the derivative of the eq. stress w.r.t stress tensor
            normxx(i) = dW_dgamma * l_coeff + dW_dlambda * dlam_dxx +          &
                        dW_dpsi * dpsi_dxx !OK
            normyy(i) = dW_dgamma * m_coeff + dW_dlambda * dlam_dyy +          &
                        dW_dpsi * dpsi_dyy !OK
            
            !< Plastic incompressibility assumption for shell thickness change
        
            normzz(i) = -(normxx(i) + normyy(i))
            
            normxy(i) = dW_dlambda * dlam_dxy + dW_dpsi * dpsi_dxy
            
            normyz(i) = zero
            normzx(i) = zero

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
      end subroutine yield_criterion_bbc2005
      end module yield_criterion_bbc2005_mod