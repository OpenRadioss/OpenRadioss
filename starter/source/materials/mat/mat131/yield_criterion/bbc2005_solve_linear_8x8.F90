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
!||    bbc2005_solve_linear_8x8_mod   ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_solve_linear_8x8.F90
!||--- called by ------------------------------------------------------
!||    bbc2005_calcul_coeffs          ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||====================================================================
      module bbc2005_solve_linear_8x8_mod
        implicit none
      contains
!===============================================================================
!   Helper: Basic 8x8 Linear System Solver (Gaussian Elimination)
!   Solves A * X = B. The answer is returned in X.
!===============================================================================
!||====================================================================
!||    bbc2005_solve_linear_8x8   ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_solve_linear_8x8.F90
!||--- called by ------------------------------------------------------
!||    bbc2005_calcul_coeffs      ../starter/source/materials/mat/mat131/yield_criterion/bbc2005_calcul_coeffs.F90
!||====================================================================
        subroutine bbc2005_solve_linear_8x8(A_in, B_in, X, info)
            implicit none
            real(kind=8), intent(in)  :: A_in(8,8), B_in(8)
            real(kind=8), intent(out) :: X(8)
            integer,       intent(out) :: info

            real(kind=8) :: A(8,8), B(8), factor
            integer       :: i, j, k

!            A = A_in
!            B = B_in
           
            do i = 1, 8
                 B(i) = B_in(i)
                  do j = 1, 8
                    A(i,j) = A_in(i,j)
                 end do
            end do   

            info = 0

            ! Forward Elimination OK
            do k = 1, 7
                ! Assuming non-zero diagonal for simplicity in starter               
                if (abs(A(k,k)) < 1.0E-14) then
                    info = 1
                    return
                end if
                do i = k + 1, 8
                    factor = A(i,k) / A(k,k)
                    do j = k, 8
                        A(i,j) = A(i,j) - factor * A(k,j)
                    end do
                    B(i) = B(i) - factor * B(k)
                end do
            end do

            ! Back Substitution OK
            X(8) = B(8) / A(8,8)
            do i = 7, 1, -1
                X(i) = B(i)
                do j = i + 1, 8
                    X(i) = X(i) - A(i,j) * X(j)
                end do
                X(i) = X(i) / A(i,i)
            end do

        end subroutine bbc2005_solve_linear_8x8

      end module bbc2005_solve_linear_8x8_mod