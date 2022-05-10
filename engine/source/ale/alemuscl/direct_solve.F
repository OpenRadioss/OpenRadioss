Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  DIRECT_SOLVE                  source/ale/alemuscl/direct_solve.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DIRECT_SOLVE (mat, rhs, sol) 
C-----------------------------------------------
C   D e s c r i p t i o n
C   This subroutine solves directly the 3 x 3 
C   symmetric definite positive linear system
C   mat * sol = rhs
C-----------------------------------------------

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real, INTENT(IN) ::  mat(3, 3)  ! matrix
      my_real, INTENT(IN) :: rhs(3)  ! right hand side
      my_real, INTENT(OUT) :: sol(3)  ! solution vector
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------      
      my_real :: det
      my_real :: a11, a12, a13, a22, a23, a33
C-----------------------------------------------
C  B e g i n n i n g  o f   S u b r o u t i n e
C-----------------------------------------------     
      !!! Determinant of the matrix
      det = 2.0D0 * mat(1,3) * mat(1,2) * mat(2, 3) - 
     .     mat(1,3) * mat(1,3) * mat(2,2) - mat(1,2) * mat(1,2) * mat(3,3) + 
     .     mat(1,1) * mat(2,2) * mat(3,3) - mat(1,1) * mat(2,3) * mat(2,3)
      !!! Coefficients of the inverse
      a11 = mat(2,2) * mat(3,3) - mat(2,3) * mat(2,3)
      a12 = -mat(1,2) * mat(3,3) + mat(1,3) * mat(2,3)
      a13 = mat(1,2) * mat(2,3) - mat(1,3) * mat(2,2)
      a22 = -mat(1,3) * mat(1,3) + mat(1,1) * mat(3,3)
      a23 = -mat(1,1) * mat(2,3) +mat(1,3) * mat(1,2)
      a33 = -mat(1,2) * mat(1,2) + mat(1,1) * mat(2,2)
      !!! Direct matrix / vector multiplication
      sol(1) = (a11 * rhs(1) + a12 * rhs(2) + a13 * rhs(3)) / det
      sol(2) = (a12 * rhs(1) + a22 * rhs(2) + a23 * rhs(3)) / det
      sol(3) = (a13 * rhs(1) + a23 * rhs(2) + a33 * rhs(3)) / det
C-----------------------------------------------
C   E n d  o f  S u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE DIRECT_SOLVE
