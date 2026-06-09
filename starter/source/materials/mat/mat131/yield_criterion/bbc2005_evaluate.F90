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
!||    bbc2005_evaluate_mod    ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_evaluate.F90
!||--- called by ------------------------------------------------------
!||    bbc2005_calcul_coeffs   ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||====================================================================
      module bbc2005_evaluate_mod
        implicit none

      contains
!===============================================================================
!   Dummy function for Newton Raphson calculation
!===============================================================================
!===============================================================================
!   Newton-Raphson Solver for BBC2005 8x8 Non-linear System
!===============================================================================
!===============================================================================
!   Helper: Evaluate Residuals F = Theoretical - Experimental
!===============================================================================
!||====================================================================
!||    bbc2005_evaluate        ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_evaluate.F90
!||--- called by ------------------------------------------------------
!||    bbc2005_calcul_coeffs   ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine bbc2005_evaluate(x, Yref, k_val, y_exp, F)
            use constant_mod,  only : pi
            implicit none
            real(kind=8), intent(in)  :: x(8), Yref, y_exp(8)
            integer,       intent(in)  :: k_val
            real(kind=8), intent(out) :: F(8)

            real(kind=8) :: a, b, L, M, N, P, Q, R
            real(kind=8) :: angles(3)
            real(kind=8) :: th, cos2, sin2
            real(kind=8) :: gam, lam, psi, Func_F, Func_G, lam_gam_p, lam_gam_m
            real(kind=8) :: term_NP, term_QR
            real(kind=8) :: lam_psi_p, lam_psi_m
            integer       :: i, k2, k2m1

            
            

            a = x(1)
            b = x(2) 
            L = x(3)
            M = x(4)
            N = x(5)
            P = x(6)
            Q = x(7)
            R = x(8)

            angles(1) = 0.0 ! 0 degrees RD direction    
            angles(2) = pi / 4.0  ! 45 degrees between RD and TD
            angles(3) = pi / 2.0  ! 90 degrees TD direction

            k2 = 2 * k_val !2*k

            k2m1 = k2 - 1  !(2*k - 1)

            ! Loop over 0, 45, 90 degrees
            do i = 1, 3
                th = angles(i)
                cos2 = cos(th) * cos(th) ! cos^2(theta)
                sin2 = sin(th) * sin(th) ! sin^2(theta)
                
                gam = L * cos2 + M * sin2 ! gamma(theta) = L*cos^2 + M*sin^2 
                term_NP = N * cos2 - P * sin2 ! term_NP(theta) = N*cos^2 - P*sin^2
                term_QR = Q * cos2 - R * sin2 ! term_QR(theta) = Q*cos^2 - R*sin^2

                lam = sqrt(term_NP * term_NP + sin2 * cos2) ! lambda(theta) = sqrt(term_NP^2 + sin^2*cos^2)
                psi = sqrt(term_QR * term_QR + sin2 * cos2) ! psi(theta) = sqrt(term_QR^2 + sin^2*cos^2)

                ! Ensure positive bases for power
                lam_gam_p = max(lam + gam, 0.0)  ! First term of F : (lambda + gamma)
                lam_gam_m = max(lam - gam, 0.0)  ! Second term of F : (lambda + gamma)     
                lam_psi_p = max(lam + psi, 0.0)  ! Third term of F : (lambda + gamma)
                lam_psi_m = max(lam - psi, 0.0)  ! Forth term of F : (lambda + gamma)

                ! Evaluate F(theta)
                Func_F = a * (lam_gam_p**k2 + lam_gam_m**k2) + &
                         b * (lam_psi_p**k2 + lam_psi_m**k2)
                Func_F = Func_F**(1.0 / real(k2))

                ! Evaluate G(theta)
                Func_G = a * (((N-P)*term_NP/max(lam,1.E-16) + L + M) * lam_gam_p**k2m1 + &
                              ((N-P)*term_NP/max(lam,1.E-16) - L - M) * lam_gam_m**k2m1 ) + &
                         b * (((N-P)*term_NP/max(lam,1.E-16) + (Q-R)*term_QR/max(psi,1.E-16)) * lam_psi_p**k2m1 + &
                              ((N-P)*term_NP/max(lam,1.E-16) - (Q-R)*term_QR/max(psi,1.E-16)) * lam_psi_m**k2m1 )

                ! Residuals for Y_theta and r_theta
!                F(i)   = (Yref / Func_F) - y_exp(i)
                 F(i)   = Func_F - (Yref / y_exp(i))
!                F(i+3) = (Func_F**k2 / Func_G - 1.0) - y_exp(i+3)
                F(i+3) = Func_F**k2 - (y_exp(i+3)+1.0)*Func_G
            end do 

            ! Biaxial State (b)
            gam = L + M
            lam = abs(N - P)
            psi = abs(Q - R)

            lam_gam_p = max(lam + gam, 0.0)
            lam_gam_m = max(lam - gam, 0.0)
            lam_psi_p = max(lam + psi, 0.0)
            lam_psi_m = max(lam - psi, 0.0)

            Func_F = a * (lam_gam_p**k2 + lam_gam_m**k2) + &
                     b * (lam_psi_p**k2 + lam_psi_m**k2)
            Func_F = Func_F**(1.0 / real(k2))

            Func_G = a * ((N*(N-P)/max(lam,1.E-16) + L) * lam_gam_p**k2m1 + &
                          (N*(N-P)/max(lam,1.E-16) - L) * lam_gam_m**k2m1 ) + &
                     b * ((N*(N-P)/max(lam,1.E-16) + Q*(Q-R)/max(psi,1.E-16)) * lam_psi_p**k2m1 + &
                          (N*(N-P)/max(lam,1.E-16) - Q*(Q-R)/max(psi,1.E-16)) * lam_psi_m**k2m1 )

!            F(7) = (Yref / Func_F) - y_exp(7)
             F(7)   = Func_F - (Yref / y_exp(7))
!            F(8) = (Func_F**k2 / Func_G - 1.0) - y_exp(8)
             F(8) = Func_F**k2 - (y_exp(8)+1.0)*Func_G
            

        end subroutine bbc2005_evaluate

!===============================================================================
!   Helper: Basic 8x8 Linear System Solver (Gaussian Elimination)
!   Solves A * X = B. The answer is returned in X.
!===============================================================================
      end module bbc2005_evaluate_mod