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
Chd|  S6FRACA                       source/elements/thickshell/solide6c/s6fraca3.F
Chd|-- called by -----------
Chd|        S6MASS3                       source/elements/thickshell/solide6c/s6mass3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6FRACA(X,IX1 ,IX2,IX3 ,IX4 ,IX5 ,IX6 ,PTG   ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr21_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)  :: NEL
      INTEGER ,DIMENSION(MVSIZ) ,INTENT(IN)  :: 
     .                  IX1, IX2, IX3, IX4, IX5 ,IX6
      my_real, DIMENSION(MVSIZ,3) , INTENT(OUT)  :: PTG 
      my_real, DIMENSION(3,NUMNOD) , INTENT(IN)  :: X 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real, DIMENSION(MVSIZ)   :: X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3 
      my_real
     .   P1, P2, P3, AA, BB, CC, A2, B2, C2,FAC
C=======================================================================
      IF (IMAS_DS>0) THEN
        DO I=1,NEL
          X1(I) = HALF*(X(1,IX1(I))+X(1,IX4(I)))
          Y1(I) = HALF*(X(2,IX1(I))+X(2,IX4(I)))
          Z1(I) = HALF*(X(3,IX1(I))+X(3,IX4(I)))
          X2(I) = HALF*(X(1,IX2(I))+X(1,IX5(I)))
          Y2(I) = HALF*(X(2,IX2(I))+X(2,IX5(I)))
          Z2(I) = HALF*(X(3,IX2(I))+X(3,IX5(I)))
          X3(I) = HALF*(X(1,IX3(I))+X(1,IX6(I)))
          Y3(I) = HALF*(X(2,IX3(I))+X(2,IX6(I)))
          Z3(I) = HALF*(X(3,IX3(I))+X(3,IX6(I)))
        END DO
        FAC = THREE/PI        
        DO I=1,NEL
          A2 = (X2(I)-X1(I))**2+(Y2(I)-Y1(I))**2+(Z2(I)-Z1(I))**2
          AA = SQRT(A2)
          B2 = (X2(I)-X3(I))**2+(Y2(I)-Y3(I))**2+(Z2(I)-Z3(I))**2
          BB = SQRT(B2)
          C2 = (X3(I)-X1(I))**2+(Y3(I)-Y1(I))**2+(Z3(I)-Z1(I))**2
          CC = SQRT(C2)
          P1 = ACOS((A2 + C2 - B2)/(TWO * AA * CC)) 
          P2 = ACOS((A2 + B2 - C2)/(TWO * AA * BB)) 
          P3 = ACOS((B2 + C2 - A2)/(TWO * BB * CC)) 
          PTG(I,1)=FAC*P1
          PTG(I,2)=FAC*P2
          PTG(I,3)=FAC*P3
        END DO
      ELSE
        PTG(1:NEL,1:3)=ONE
      END IF
C-----------
      RETURN
      END SUBROUTINE S6FRACA
