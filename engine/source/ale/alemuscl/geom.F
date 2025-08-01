Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    geom                            ../engine/source/ale/alemuscl/geom.F
!||--- called by ------------------------------------------------------
!||    ale51_gradient_reconstruction   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!||    centroid3t                      ../engine/source/multifluid/centroid.F
!||====================================================================
      SUBROUTINE GEOM(A, B, C, CENTER_X,CENTER_Y,CENTER_Z, VOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "implicit_f.inc"  
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------- 
      my_real, INTENT(IN) :: A(3), B(3), C(3)
      my_real, INTENT(INOUT) :: CENTER_X,CENTER_Y,CENTER_Z, VOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: NORM(3), AB, BC, CA
C-----------------------------------------------
C   S o u r c e   L i n e s 
C-----------------------------------------------

      NORM(1) = (B(2) - A(2)) * (C(3) - A(3)) - (B(3) - A(3)) * (C(2) - A(2))
      NORM(2) = (B(3) - A(3)) * (C(1) - A(1)) - (B(1) - A(1)) * (C(3) - A(3))
      NORM(3) = (B(1) - A(1)) * (C(2) - A(2)) - (B(2) - A(2)) * (C(1) - A(1))
      
      VOL = VOL + DOT_PRODUCT(A, NORM)
      
      AB = DOT_PRODUCT(HALF * (A + B), HALF * (A + B))
      BC = DOT_PRODUCT(HALF * (B + C), HALF * (B + C))
      CA = DOT_PRODUCT(HALF * (C + A), HALF * (C + A))

      CENTER_X = CENTER_X + NORM(1) * (AB + BC + CA)
      CENTER_Y = CENTER_Y + NORM(2) * (AB + BC + CA)
      CENTER_Z = CENTER_Z + NORM(3) * (AB + BC + CA)

C-----------------------------------------------      
      END SUBROUTINE GEOM
