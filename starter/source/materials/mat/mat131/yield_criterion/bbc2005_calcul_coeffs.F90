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
!||    bbc2005_calcul_coeffs_mod         ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion_bbc2005   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_BBC2005.F90
!||====================================================================
      module bbc2005_calcul_coeffs_mod
        implicit none
! \brief Read BBC2005 yield criterion input data for /MAT/LAW131
! \details Read the BBC2005 anisotropic yield criterion parameters
!          for /MAT/LAW131.
      contains

!===============================================================================
!   Dummy function for Newton Raphson calculation
!===============================================================================
!===============================================================================
!   Newton-Raphson Solver for BBC2005 8x8 Non-linear System
!===============================================================================
!||====================================================================
!||    bbc2005_calcul_coeffs             ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||--- called by ------------------------------------------------------
!||    hm_read_yield_criterion_bbc2005   ../starter/source/materials/mat/mat131/yield_criterion/hm_read_yield_criterion_BBC2005.F90
!||--- calls      -----------------------------------------------------
!||    bbc2005_evaluate                  ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_evaluate.F90
!||    bbc2005_solve_linear_8x8          ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_solve_linear_8x8.F90
!||--- uses       -----------------------------------------------------
!||    bbc2005_evaluate_mod              ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_evaluate.F90
!||    bbc2005_solve_linear_8x8_mod      ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_solve_linear_8x8.F90
!||====================================================================
        subroutine bbc2005_calcul_coeffs(y0, y45, y90, r0, r45, r90, yb, rb, k_val, &
                                         a, b, l_coeff, m_coeff, n_coeff, p, q, r_coeff)
            use precision_mod, only : WP
            use bbc2005_evaluate_mod
            USE bbc2005_solve_linear_8x8_mod
            implicit none
            real(kind=8), intent(in)  :: y0, y45, y90, r0, r45, r90, yb, rb
            integer,       intent(in)  :: k_val
            real(kind=WP), intent(out) :: a, b, l_coeff, m_coeff, n_coeff, p, q, r_coeff

            real(kind=8) :: x(8), dx(8), F(8), Jac(8,8)
            real(kind=8) :: F_plus(8)
            real(kind=8) :: F_moins(8)
            real(kind=8) :: y_exp(8)
            real(kind=8) :: eps, err_norm
            integer       :: iter, max_iter, i, j, info

            ! 1. Setup target experimental array
            y_exp(1) = y0
            y_exp(2) = y45
            y_exp(3) = y90
            y_exp(4) = r0
            y_exp(5) = r45
            y_exp(6) = r90
            y_exp(7) = yb
            y_exp(8) = rb

            ! 2. Initial Guess for [a, b, L, M, N, P, Q, R]
            x(1) = 0.7  ! a
            x(2) = 0.3  ! b
            x(3) = 0.51  ! L
            x(4) = 0.52  ! M
            x(5) = 0.48  ! N
            x(6) = 0.49  ! P
            x(7) = 0.47  ! Q
            x(8) = 0.53  ! R

            max_iter = 50
            eps = 1.0E-8
            info = 0

            ! 3. Newton-Raphson Loop
            do iter = 1, max_iter
                ! Evaluate Residual F(x) = Y_th(x) - Y_exp
                call bbc2005_evaluate(x, y0, k_val, y_exp, F) 

                err_norm = 0.0
                do i = 1, 8
                    err_norm = err_norm + abs(F(i))
                end do
                
                ! Check convergence
                if (err_norm < eps) exit

                ! Compute Numerical Jacobian Jac = dF/dx using Forward Difference
                do j = 1, 8
                    x(j) = x(j) + eps
                    call bbc2005_evaluate(x, y0, k_val, y_exp, F_plus)
                    x(j) = x(j) - eps  ! Restore x
                    x(j) = x(j) - eps
                    call bbc2005_evaluate(x, y0, k_val, y_exp, F_moins)
                    x(j) = x(j) + eps
                    do i = 1, 8
                        Jac(i, j) = (F_plus(i) - F_moins(i)) / (2.0 * eps)
                    end do
                end do

                ! Solve Jac * dx = -F using a basic 8x8 Linear Solver
                ! JAC(I,J) = dF(I)/dX(J)
                call bbc2005_solve_linear_8x8(Jac, F, dx, info)

                if (info /= 0) then
                    ! Fallback to identity if singular (very rare if guess is good)
                    x = 1.0
                    exit
                end if

                ! Update x = x - dx (Since we solved Jac*dx = F, we subtract)
                do i = 1, 8
                    x(i) = x(i) - 0.1*dx(i)
                end do


            end do

            ! 4. Map solution back to variables
            a       = x(1)
            b       = x(2)
            l_coeff = x(3)
            m_coeff = x(4)
            n_coeff = x(5)
            p       = x(6)
            q       = x(7)
            r_coeff = x(8)

        end subroutine bbc2005_calcul_coeffs
      end module bbc2005_calcul_coeffs_mod
