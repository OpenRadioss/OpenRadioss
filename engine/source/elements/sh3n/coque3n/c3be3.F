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
!||    c3be3   ../engine/source/elements/sh3n/coque3n/c3be3.F
!||--- called by ------------------------------------------------------
!||    c3ke3   ../engine/source/elements/sh3n/coque3n/c3ke3.F
!||====================================================================
      SUBROUTINE C3BE3(JFT,JLT,PX1,PY1,PY2 ,
     1                   X2 ,X3 , Y3 , AREA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
      my_real PX1(*), PY1(*), PY2(*), X2(*),X3(*), Y3(*),AREA(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real AI   
C-----------------------------------------------
C--px2=-px1;px3=0;py3=-py1-py2---
      DO I=JFT,JLT
       AI = HALF/MAX(AREA(I),EM20)
       PX1(I)=-AI*Y3(I)
       PY1(I)= AI*(X3(I)-X2(I))
       PY2(I)=-AI*X3(I)      
      ENDDO
C---------------------------------------------------------
      RETURN
C
      END
