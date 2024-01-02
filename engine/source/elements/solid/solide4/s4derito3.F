Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  S4DERITO3                     source/elements/solid/solide4/s4derito3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4DERITO3(
     1   OFF,     DET,     X1,      X2,
     2   X3,      X4,      Y1,      Y2,
     3   Y3,      Y4,      Z1,      Z2,
     4   Z3,      Z4,      PX1,     PX2,
     5   PX3,     PX4,     PY1,     PY2,
     6   PY3,     PY4,     PZ1,     PZ2,
     7   PZ3,     PZ4,     RX,      RY,
     8   RZ,      SX,      SY,      SZ,
     9   TX,      TY,      TZ,      JAC_I,
     A   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      my_real
     .   OFF(*),DET(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),TX(*), TY(*), TZ(*),
     .   JAC_I(10,*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C     REAL
C                                                                     12
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
      my_real
     .   JI1, JI2, JI3,  
     .   JI4, JI5, JI6,  
     .   JI7, JI8, JI9,  
     .   A1, A2, A3, A4, D, AREAMX2
C-----------------------------------------------
      DO I=1,NEL
       X43 = X4(I) - X3(I)
       Y43 = Y4(I) - Y3(I)
       Z43 = Z4(I) - Z3(I)
       X41 = X4(I) - X1(I)
       Y41 = Y4(I) - Y1(I)
       Z41 = Z4(I) - Z1(I)
       X42 = X4(I) - X2(I)
       Y42 = Y4(I) - Y2(I)
       Z42 = Z4(I) - Z2(I)
C
       RX(I) =  -X41
       RY(I) =  -Y41
       RZ(I) =  -Z41
       SX(I) =  -X42
       SY(I) =  -Y42
       SZ(I) =  -Z42
       TX(I) =  -X43
       TY(I) =  -Y43
       TZ(I) =  -Z43
C
      ENDDO
C
      DO I=1,NEL
       JI1 = JAC_I(1,I)
       JI2 = JAC_I(2,I)
       JI3 = JAC_I(3,I)
       JI4 = JAC_I(4,I)
       JI5 = JAC_I(5,I)
       JI6 = JAC_I(6,I)
       JI7 = JAC_I(7,I)
       JI8 = JAC_I(8,I)
       JI9 = JAC_I(9,I)
C
        PX1(I)= JI1
        PY1(I)= JI4
        PZ1(I)= JI7
        PX2(I)= JI2
        PY2(I)= JI5
        PZ2(I)= JI8
        PX3(I)= JI3
        PY3(I)= JI6
        PZ3(I)= JI9
        PX4(I)=-(JI1+JI2+JI3)
        PY4(I)=-(JI4+JI5+JI6)
        PZ4(I)=-(JI7+JI8+JI9)
      ENDDO
C
      RETURN
C
      END
