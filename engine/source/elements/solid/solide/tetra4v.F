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
Chd|  TETRA4V                       source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        DEGES4V                       source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TETRA4V(DET,
     .   X1, X2, X3, X4, 
     .   Y1, Y2, Y3, Y4,
     .   Z1, Z2, Z3, Z4)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   DET,
     .   X1, X2, X3, X4, Y1, Y2, Y3, Y4,Z1, Z2, Z3, Z4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C                                                                     12
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
      my_real
     .   RX, RY, RZ, SX, SY, SZ,TX, TY, TZ,
     .   TSX, TSY,TSZ
C-----------------------------------------------
       X43 = X4 - X3
       Y43 = Y4 - Y3
       Z43 = Z4 - Z3
       X41 = X4 - X1
       Y41 = Y4 - Y1
       Z41 = Z4 - Z1
       X42 = X4 - X2
       Y42 = Y4 - Y2
       Z42 = Z4 - Z2
C
       RX =  -X41
       RY =  -Y41
       RZ =  -Z41
C
       SX =  -X42
       SY =  -Y42
       SZ =  -Z42
C
       TX =  -X43
       TY =  -Y43
       TZ =  -Z43
C
       TSX =  Y43*Z42 - Y42*Z43
       TSY =  Z43*X42 - Z42*X43
       TSZ =  X43*Y42 - X42*Y43
C------V=r*(s^t)/6
       DET = ABS(RX*TSX + RY*TSY + RZ*TSZ)*ONE_OVER_6
C      
      RETURN
      END
