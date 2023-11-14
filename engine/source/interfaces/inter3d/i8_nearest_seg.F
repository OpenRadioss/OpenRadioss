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
Chd|  I8_NEAREST_SEG                source/interfaces/inter3d/i8_nearest_seg.F
Chd|-- called by -----------
Chd|        I8MSR3                        source/interfaces/inter3d/i8msr3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I8_NEAREST_SEG(X, IS, M1, M2, M3, BMIN)
C  
C  COMPUTE BARYCENTRIC COORDINATES OF PROJECTION OF NODE IS ON TRIANGLE M1 M2 M3
C  WOLFGANG HEIDRICH, 2005
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: IS, M1, M2, M3
      my_real, INTENT(IN)  :: X(3,*)
      my_real, INTENT(OUT) :: BMIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real N(3), U(3), V(3), W(3), UW(3), WV(3)
      my_real B0, B1, B2, FAC
C---------------------------------------------------
      U(1)=X(1,M2)-X(1,M1)
      U(2)=X(2,M2)-X(2,M1)
      U(3)=X(3,M2)-X(3,M1)
C
      V(1)=X(1,M3)-X(1,M1)
      V(2)=X(2,M3)-X(2,M1)
      V(3)=X(3,M3)-X(3,M1)
C
      N(1)=U(2)*V(3)-U(3)*V(2)
      N(2)=U(3)*V(1)-U(1)*V(3)
      N(3)=U(1)*V(2)-U(2)*V(1)
C
      W(1)=X(1,IS)-X(1,M1)
      W(2)=X(2,IS)-X(2,M1)
      W(3)=X(3,IS)-X(3,M1)
C
      UW(1)=U(2)*W(3)-U(3)*W(2)
      UW(2)=U(3)*W(1)-U(1)*W(3)
      UW(3)=U(1)*W(2)-U(2)*W(1)
C
      WV(1)=W(2)*V(3)-W(3)*V(2)
      WV(2)=W(3)*V(1)-W(1)*V(3)
      WV(3)=W(1)*V(2)-W(2)*V(1)
C
      FAC=ONE/(N(1)*N(1)+N(2)*N(2)+N(3)*N(3))
      B2 =(UW(1)*N(1)+UW(2)*N(2)+UW(3)*N(3))*FAC
      B1 =(WV(1)*N(1)+WV(2)*N(2)+WV(3)*N(3))*FAC
      B0 = ONE - B1 - B2
      BMIN=MIN(B0,B1,B2)    
C BMIN > 0 Projection est a l'interieur du triangle
C BMIN = 0 Projection est sur un cote du triangle
C BMIN < 0 Projection est a l'exterieur du triangle
      RETURN
      END
