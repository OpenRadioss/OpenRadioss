!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
!Chd|====================================================================
!Chd|  CHOLESKY_SOLVE_Q1NP            source/elements/solid/solid_q1np/q1np_cholesky.F90
!Chd|====================================================================
!=======================================================================
!   Solve A * X = B where A is symmetric positive definite (N x N).
!   A is overwritten with Cholesky factor L (lower). B and X are N x NRHS.
!   NRHS is the number of right-hand sides (3 for 3D surface fit).
!=======================================================================
      module q1np_cholesky_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! result in X
        subroutine cholesky_solve_q1np(n, a, lda, b, nrhs, x)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod, only : ZERO
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                   intent(in)    :: n
          integer,                   intent(in)    :: lda
          integer,                   intent(in)    :: nrhs
          real(kind=WP),             intent(inout) :: a(lda, n)
          real(kind=WP),             intent(in)    :: b(lda, nrhs)
          real(kind=WP),             intent(out)   :: x(lda, nrhs)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: j, i, k, ix
          real(kind=WP) :: sum_val
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!   Cholesky: A = L * L^T, store L in lower part of A
! ----------------------------------------------------------------------------------------------------------------------
          do j = 1, n
            sum_val = a(j, j)
            do k = 1, j - 1
              sum_val = sum_val - a(j, k) * a(j, k)
            end do
            if (sum_val <= ZERO) then
              a(j, j) = ZERO
              print *, 'Q1NP Cholesky: FAILED at column J = ', j, ' SUM = ', sum_val
              return
            end if
            a(j, j) = sqrt(sum_val)
            do i = j + 1, n
              sum_val = a(i, j)
              do k = 1, j - 1
                sum_val = sum_val - a(i, k) * a(j, k)
              end do
              a(i, j) = sum_val / a(j, j)
            end do
          end do
! ----------------------------------------------------------------------------------------------------------------------
!   Solve L * Y = B (forward substitution), store Y in X
! ----------------------------------------------------------------------------------------------------------------------
          do ix = 1, nrhs
            do i = 1, n
              sum_val = b(i, ix)
              do k = 1, i - 1
                sum_val = sum_val - a(i, k) * x(k, ix)
              end do
              x(i, ix) = sum_val / a(i, i)
            end do
          end do
! ----------------------------------------------------------------------------------------------------------------------
!   Solve L^T * X = Y (back substitution; overwrite X with solution)
! ----------------------------------------------------------------------------------------------------------------------
          do ix = 1, nrhs
            do i = n, 1, -1
              sum_val = x(i, ix)
              do k = i + 1, n
                sum_val = sum_val - a(k, i) * x(k, ix)
              end do
              x(i, ix) = sum_val / a(i, i)
            end do
          end do
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine cholesky_solve_q1np
      end module q1np_cholesky_mod
