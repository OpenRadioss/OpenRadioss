Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  CG                            source/ale/alemuscl/conjugate_gradient.F
Chd|-- called by -----------
Chd|        GRADIENT_RECONSTRUCTION       source/ale/alemuscl/gradient_reconstruction.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CG (dim, mat, rhs, sol, max_iter, tol) 
C-----------------------------------------------
C   D e s c r i p t i o n
C   This subroutine computes the solution to the linear system
C   mat * sol = rhs by the conjugate gradient method.
C   This assumes that mat is a symmetric positive matrix
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) ::  dim  ! system dimension
      my_real, INTENT(INOUT) ::  mat(dim, dim)  ! matrix
      my_real, INTENT(INOUT) :: rhs(dim)  ! right hand side
      my_real, INTENT(OUT) :: sol(dim)  ! solution vector
      INTEGER, INTENT(IN) :: max_iter
      my_real, INTENT(IN) :: tol
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ii  ! matrix and vector indexes
      INTEGER iter  ! iteration count
      my_real error, norm_init  ! error, initial norm 
      my_real :: r(dim), rnew(dim), p(dim), temp(dim)
      my_real :: alpha, beta
C-----------------------------------------------
C   S o u r c e   L i n e s 
C----------------------------------------------- 
!!!   Setting the solution vector to zero
      sol(1:dim) = ZERO
!!!   Initialization of the algorithm
      r(1:dim) = rhs(1:dim) - MATMUL(mat(1:dim, 1:dim), sol(1:dim))
      p(1:dim) = r(1:dim)
      norm_init = MAXVAL(ABS(r(1:dim)))
      error = norm_init
      iter = 0

!!!   Main loop
      DO WHILE ((iter < max_iter) .AND. (error > tol))
      !DO WHILE (iter < max_iter)
         iter = iter + 1
         temp(1:dim) = MATMUL(mat(1:dim, 1:dim), p(1:dim))
         alpha = DOT_PRODUCT(r(1:dim), r(1:dim)) / DOT_PRODUCT(temp(1:dim), p(1:dim))
         DO ii = 1, dim
            sol(ii) = sol(ii) + alpha * p(ii)
            rnew(ii) = r(ii) - alpha * temp(ii)
         ENDDO
         beta = DOT_PRODUCT(rnew(1:dim), rnew(1:dim)) / DOT_PRODUCT(r(1:dim), r(1:dim))
         DO ii = 1, dim
            p(ii) = rnew(ii) + beta * p(ii)
            r(ii) = rnew(ii)
         ENDDO
         error = MAXVAL(ABS(r(1:dim))) / norm_init
      ENDDO

      IF (error > tol) THEN
C         PRINT*, "GC NON CONVERGENCE"
      ENDIF

C-----------------------------------------------
      END SUBROUTINE CG
