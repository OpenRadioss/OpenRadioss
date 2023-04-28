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
Chd|  T4DERI3                       source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- called by -----------
Chd|        T4_TSTRAIN                    source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE T4DERI3(
     .   X1,  X2,  X3,  X4,  Y1,  Y2,  Y3,  Y4, 
     .   Z1,  Z2,  Z3,  Z4,  PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4, 
     .   DET  ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
      my_real
     .   X1(*), X2(*), X3(*), X4(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
     
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),DET(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      INTEGER I
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,
     .   A1, A2, A3, A4, D, AREAMX2, 
     .   B1(NEL), B2(NEL), B3(NEL), B4(NEL), 
     .   C1(NEL), C2(NEL), C3(NEL), C4(NEL),
     .   D1(NEL), D2(NEL), D3(NEL), D4(NEL)
C-----------------------------------------------
C
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
       B1(I) =  Y43*Z42 - Y42*Z43
       B2(I) =  Y41*Z43 - Y43*Z41
       B3(I) =  Y42*Z41 - Y41*Z42
       B4(I) =  -(B1(I) + B2(I) + B3(I))
C
       C1(I) =  Z43*X42 - Z42*X43
       C2(I) =  Z41*X43 - Z43*X41
       C3(I) =  Z42*X41 - Z41*X42
       C4(I) =  -(C1(I) + C2(I) + C3(I))
C
       D1(I) =  X43*Y42 - X42*Y43
       D2(I) =  X41*Y43 - X43*Y41
       D3(I) =  X42*Y41 - X41*Y42
       D4(I) =  -(D1(I) + D2(I) + D3(I))
C----------SIX = 6.
       DET(I) = (X41*B1(I) + Y41*C1(I) + Z41*D1(I))/SIX
C
      ENDDO
C
      DO I=1,NEL
        D = ONE/DET(I)/SIX
        PX1(I)=-B1(I)*D
        PY1(I)=-C1(I)*D
        PZ1(I)=-D1(I)*D
        PX2(I)=-B2(I)*D
        PY2(I)=-C2(I)*D
        PZ2(I)=-D2(I)*D
        PX3(I)=-B3(I)*D
        PY3(I)=-C3(I)*D
        PZ3(I)=-D3(I)*D
        PX4(I)=-B4(I)*D
        PY4(I)=-C4(I)*D
        PZ4(I)=-D4(I)*D
       END DO
C
      RETURN
      END
