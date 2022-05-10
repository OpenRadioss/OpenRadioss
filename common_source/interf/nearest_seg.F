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
Chd|  NEAREST_SEG                   interf/nearest_seg.F          
Chd|-- called by -----------
Chd|        INVOI3                        starter/source/interfaces/inter3d1/invoi3.F
Chd|        I3MSR3                        engine/source/interfaces/inter3d/i3msr3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NEAREST_SEG(X, IS, M1, M2, M3, LSEG, LSEG_NEW, BMIN, BMAX)
Cfa
C  COMPUTE BARYCENTRIC COORDINATES OF PROJECTION OF NODE IS ON TRIANGLE M1 M2 M3
C  WOLFGANG HEIDRICH, 2005
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN)  :: IS, M1, M2, M3, LSEG
      INTEGER, INTENT(OUT) :: LSEG_NEW 
      my_real, INTENT(IN)  :: X(3,*)
      my_real, INTENT(OUT) :: BMIN
      my_real, INTENT(INOUT) :: BMAX
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
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
      FAC=UN/(N(1)*N(1)+N(2)*N(2)+N(3)*N(3))
      B2 =(UW(1)*N(1)+UW(2)*N(2)+UW(3)*N(3))*FAC
      B1 =(WV(1)*N(1)+WV(2)*N(2)+WV(3)*N(3))*FAC
      B0 = UN - B1 - B2
      BMIN=MIN(B0,B1,B2)	  
C BMIN > 0 Projection est a l'interieur du triangle
C BMIN = 0 Projection est sur un cote du triangle
C BMIN < 0 Projection est a l'exterieur du triangle
      IF(BMIN > BMAX) THEN
         LSEG_NEW=LSEG
         BMAX=BMIN
      ENDIF
      RETURN
      END
