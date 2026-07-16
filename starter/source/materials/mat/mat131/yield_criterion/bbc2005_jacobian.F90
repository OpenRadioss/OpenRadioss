!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
module bbc2005_jacobian_mod
    implicit none

contains

!||====================================================================
!||    bbc2005_jacobian        ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_jacobian.F90
!||--- called by ------------------------------------------------------
!||    bbc2005_calcul_coeffs   ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
    subroutine bbc2005_jacobian(x, k_val, y_exp, Jac)
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use constant_mod, only : pi
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        real(kind=8), intent(in)  :: x(8)                   !< BBC2005 coefficients
        integer,      intent(in)  :: k_val                  !< yield criterion exponent parameter
        real(kind=8), intent(in)  :: y_exp(8)               !< experimental data
        real(kind=8), intent(out) :: Jac(8,8)               !< Jacobian matrix
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        real(kind=8) :: a, b, L, M, N, P, Q, R
        real(kind=8) :: angles(3), th, cos2, sin2
        real(kind=8) :: gam, lam, psi, lam_gam_p, lam_gam_m, lam_psi_p, lam_psi_m
        real(kind=8) :: term_NP, term_QR, W_val, K1p, K1m, K2p, K2m
        integer       :: i, j, k2, k2m1
        real(kind=8) :: d_gam_dx(8), d_term_NP_dx(8), d_term_QR_dx(8)
        real(kind=8) :: d_lam_dx(8), d_psi_dx(8)
        real(kind=8) :: d_lgp_dx(8), d_lgm_dx(8), d_lpp_dx(8), d_lpm_dx(8)
        real(kind=8) :: d_NP_term_dx(8), d_QR_term_dx(8)
        real(kind=8) :: d_K1p_dx(8), d_K1m_dx(8), d_K2p_dx(8), d_K2m_dx(8)
        real(kind=8) :: d_W_dx(8), d_Func_F_dx(8), d_Func_G_dx(8)
        real(kind=8) :: d_N_minus_P, d_Q_minus_R, d_N, d_Q

        a = x(1)
        b = x(2)
        L = x(3)
        M = x(4)
        N = x(5)
        P = x(6)
        Q = x(7)
        R = x(8)

        angles(1) = 0.0 
        angles(2) = pi / 4.0 
        angles(3) = pi / 2.0 

        k2 = 2 * k_val
        k2m1 = k2 - 1

        ! Initialize Jacobian matrix
        Jac = 0.0 

        ! =====================================================================
        ! First part: uniaxial states (0, 45, 90 degrees)
        ! =====================================================================
        do i = 1, 3
            th = angles(i)
            cos2 = cos(th) * cos(th)
            sin2 = sin(th) * sin(th)
            
            gam = L * cos2 + M * sin2
            term_NP = N * cos2 - P * sin2
            term_QR = Q * cos2 - R * sin2

            ! [Core Fix 1] Global smoothing of uniaxial absolute value, prevent N, P singularities in RD and TD directions
            lam = sqrt(term_NP * term_NP + sin2 * cos2 + 1.0E-12 )
            psi = sqrt(term_QR * term_QR + sin2 * cos2 + 1.0E-12 )

            ! [Core Fix 2] Completely remove max() function, restore natural mathematical expressions
            lam_gam_p = lam + gam
            lam_gam_m = lam - gam
            lam_psi_p = lam + psi
            lam_psi_m = lam - psi

            ! 1. Base term partial derivatives
            d_gam_dx = 0.0
            d_gam_dx(3) = cos2
            d_gam_dx(4) = sin2
            d_term_NP_dx = 0.0
            d_term_NP_dx(5) = cos2
            d_term_NP_dx(6) = -sin2
            d_term_QR_dx = 0.0
            d_term_QR_dx(7) = cos2
            d_term_QR_dx(8) = -sin2

            ! 2. Core radius term partial derivatives (denominator is safe with added 1E-12, no if needed)
            d_lam_dx = (term_NP / lam) * d_term_NP_dx
            d_psi_dx = (term_QR / psi) * d_term_QR_dx

            d_lgp_dx = d_lam_dx + d_gam_dx
            d_lgm_dx = d_lam_dx - d_gam_dx
            d_lpp_dx = d_lam_dx + d_psi_dx
            d_lpm_dx = d_lam_dx - d_psi_dx

            ! 3. Compute W partial derivatives
            W_val = a * (lam_gam_p**k2 + lam_gam_m**k2) + b * (lam_psi_p**k2 + lam_psi_m**k2)
            
            d_W_dx = 0.0 
            d_W_dx(1) = lam_gam_p**k2 + lam_gam_m**k2
            d_W_dx(2) = lam_psi_p**k2 + lam_psi_m**k2
            do j = 3, 8
                d_W_dx(j) = a * k2 * (lam_gam_p**k2m1 * d_lgp_dx(j) + lam_gam_m**k2m1 * d_lgm_dx(j)) + &
                            b * k2 * (lam_psi_p**k2m1 * d_lpp_dx(j) + lam_psi_m**k2m1 * d_lpm_dx(j))
            end do

            ! 4. Compute partial derivatives of Func_F
            d_Func_F_dx = (1.0  / real(k2, 8)) * W_val**(1.0 /real(k2, 8) - 1.0 ) * d_W_dx

            ! 5. Partial derivatives of K multipliers inside G function (remove if-guards, embrace smooth derivatives)
            d_NP_term_dx = 0.0 
            do j = 1, 8
                d_N_minus_P = 0.0 
                if (j == 5) d_N_minus_P = 1.0 
                if (j == 6) d_N_minus_P = -1.0 
                d_NP_term_dx(j) = (d_N_minus_P * term_NP / lam) + &
                                  ((N-P) * d_term_NP_dx(j) / lam) - &
                                  ((N-P) * term_NP / (lam**2) * d_lam_dx(j))
            end do

            d_QR_term_dx = 0.0 
            do j = 1, 8
                d_Q_minus_R = 0.0 
                if (j == 7) d_Q_minus_R = 1.0 
                if (j == 8) d_Q_minus_R = -1.0 
                d_QR_term_dx(j) = (d_Q_minus_R * term_QR / psi) + &
                                  ((Q-R) * d_term_QR_dx(j) / psi) - &
                                  ((Q-R) * term_QR / (psi**2) * d_psi_dx(j))
            end do

            ! Combine K multipliers and their derivatives (remove all max() restrictions)
            K1p = (N-P)*term_NP/lam + L + M
            K1m = (N-P)*term_NP/lam - L - M
            K2p = (N-P)*term_NP/lam + (Q-R)*term_QR/psi
            K2m = (N-P)*term_NP/lam - (Q-R)*term_QR/psi

            d_K1p_dx = d_NP_term_dx
            d_K1p_dx(3) = d_K1p_dx(3) + 1.0
            d_K1p_dx(4) = d_K1p_dx(4) + 1.0
            d_K1m_dx = d_NP_term_dx
            d_K1m_dx(3) = d_K1m_dx(3) - 1.0
            d_K1m_dx(4) = d_K1m_dx(4) - 1.0
            d_K2p_dx = d_NP_term_dx + d_QR_term_dx
            d_K2m_dx = d_NP_term_dx - d_QR_term_dx

            ! 6. Compute full partial derivatives of Func_G
            d_Func_G_dx = 0.0 
            d_Func_G_dx(1) = K1p * lam_gam_p**k2m1 + K1m * lam_gam_m**k2m1
            d_Func_G_dx(2) = K2p * lam_psi_p**k2m1 + K2m * lam_psi_m**k2m1
            do j = 3, 8
                d_Func_G_dx(j) = a * ( d_K1p_dx(j) * lam_gam_p**k2m1 + K1p * k2m1 * lam_gam_p**(k2m1-1) * d_lgp_dx(j) + &
                                       d_K1m_dx(j) * lam_gam_m**k2m1 + K1m * k2m1 * lam_gam_m**(k2m1-1) * d_lgm_dx(j) ) + &
                                 b * ( d_K2p_dx(j) * lam_psi_p**k2m1 + K2p * k2m1 * lam_psi_p**(k2m1-1) * d_lpp_dx(j) + &
                                       d_K2m_dx(j) * lam_psi_m**k2m1 + K2m * k2m1 * lam_psi_m**(k2m1-1) * d_lpm_dx(j) )
            end do

            ! 7. Fill Jacobian matrix
            do j = 1, 8
                Jac(i, j)   = d_Func_F_dx(j)
                Jac(i+3, j) = d_W_dx(j) - (y_exp(i+3) + 1.0 ) * d_Func_G_dx(j)
            end do 
        end do

        ! =====================================================================
        ! Second part: equal biaxial state (Biaxial State) 
        ! =====================================================================
        gam = L + M
        ! [Core Fix 3] Global smoothing of biaxial absolute value, fully resolve N=P singularity
        lam = sqrt((N - P)**2 + 1.0E-12 )
        psi = sqrt((Q - R)**2 + 1.0E-12 )

        ! Remove max truncation in biaxial state
        lam_gam_p = lam + gam
        lam_gam_m = lam - gam
        lam_psi_p = lam + psi
        lam_psi_m = lam - psi

        d_gam_dx = 0.0
        d_gam_dx(3) = 1.0
        d_gam_dx(4) = 1.0
        
        ! [Core Fix 4] Biaxial derivatives: use exact chain-rule partial derivatives instead of crude sign() jumps
        d_lam_dx = 0.0 
        d_lam_dx(5) =  (N - P) / lam
        d_lam_dx(6) = -(N - P) / lam
        
        d_psi_dx = 0.0 
        d_psi_dx(7) =  (Q - R) / psi
        d_psi_dx(8) = -(Q - R) / psi

        d_lgp_dx = d_lam_dx + d_gam_dx
        d_lgm_dx = d_lam_dx - d_gam_dx
        d_lpp_dx = d_lam_dx + d_psi_dx
        d_lpm_dx = d_lam_dx - d_psi_dx

        W_val = a * (lam_gam_p**k2 + lam_gam_m**k2) + b * (lam_psi_p**k2 + lam_psi_m**k2)
        
        d_W_dx = 0.0 
        d_W_dx(1) = lam_gam_p**k2 + lam_gam_m**k2
        d_W_dx(2) = lam_psi_p**k2 + lam_psi_m**k2
        do j = 3, 8
            d_W_dx(j) = a * k2 * (lam_gam_p**k2m1 * d_lgp_dx(j) + lam_gam_m**k2m1 * d_lgm_dx(j)) + &
                        b * k2 * (lam_psi_p**k2m1 * d_lpp_dx(j) + lam_psi_m**k2m1 * d_lpm_dx(j))
        end do

        d_Func_F_dx = (1.0  / real(k2, 8)) * W_val**(1.0 /real(k2, 8) - 1.0 ) * d_W_dx

        ! Biaxial G function specific K multiplier derivatives (remove if protection, trust smooth expressions)
        d_NP_term_dx = 0.0 
        do j = 1, 8
            d_N = 0.0
            if (j == 5) d_N = 1.0
            d_N_minus_P = 0.0
            if (j == 5) d_N_minus_P = 1.0
            if (j == 6) d_N_minus_P = -1.0
            d_NP_term_dx(j) = (d_N * (N-P) / lam) + &
                              (N * d_N_minus_P / lam) - &
                              (N * (N-P) / (lam**2) * d_lam_dx(j))
        end do

        d_QR_term_dx = 0.0 
        do j = 1, 8
            d_Q = 0.0
            if (j == 7) d_Q = 1.0
            d_Q_minus_R = 0.0
            if (j == 7) d_Q_minus_R = 1.0
            if (j == 8) d_Q_minus_R = -1.0
            d_QR_term_dx(j) = (d_Q * (Q-R) / psi) + &
                              (Q * d_Q_minus_R / psi) - &
                              (Q * (Q-R) / (psi**2) * d_psi_dx(j))
        end do

        ! Biaxial K multipliers (remove max truncation)
        K1p = N*(N-P)/lam + L
        K1m = N*(N-P)/lam - L
        K2p = N*(N-P)/lam + Q*(Q-R)/psi
        K2m = N*(N-P)/lam - Q*(Q-R)/psi

        d_K1p_dx = d_NP_term_dx
        d_K1p_dx(3) = d_K1p_dx(3) + 1.0
        d_K1m_dx = d_NP_term_dx
        d_K1m_dx(3) = d_K1m_dx(3) - 1.0
        d_K2p_dx = d_NP_term_dx + d_QR_term_dx
        d_K2m_dx = d_NP_term_dx - d_QR_term_dx

        d_Func_G_dx = 0.0 
        d_Func_G_dx(1) = K1p * lam_gam_p**k2m1 + K1m * lam_gam_m**k2m1
        d_Func_G_dx(2) = K2p * lam_psi_p**k2m1 + K2m * lam_psi_m**k2m1
        do j = 3, 8
            d_Func_G_dx(j) = a * ( d_K1p_dx(j) * lam_gam_p**k2m1 + K1p * k2m1 * lam_gam_p**(k2m1-1) * d_lgp_dx(j) + &
                                   d_K1m_dx(j) * lam_gam_m**k2m1 + K1m * k2m1 * lam_gam_m**(k2m1-1) * d_lgm_dx(j) ) + &
                             b * ( d_K2p_dx(j) * lam_psi_p**k2m1 + K2p * k2m1 * lam_psi_p**(k2m1-1) * d_lpp_dx(j) + &
                                   d_K2m_dx(j) * lam_psi_m**k2m1 + K2m * k2m1 * lam_psi_m**(k2m1-1) * d_lpm_dx(j) )
        end do

        ! Fill biaxial Jacobian matrix
        do j = 1, 8
            Jac(7, j) = d_Func_F_dx(j)
            Jac(8, j) = d_W_dx(j) - (y_exp(8) + 1.0 ) * d_Func_G_dx(j)
        end do

    end subroutine bbc2005_jacobian

end module bbc2005_jacobian_mod