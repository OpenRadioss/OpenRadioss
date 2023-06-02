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
Chd|  S4JACI3                       source/elements/solid/solide4/s4jaci3.F
Chd|-- called by -----------
Chd|        S4INIT3                       source/elements/solid/solide4/s4init3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4JACI3(SAV   ,JAC_I, VOL, NEL  ) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER , INTENT(IN) :: NEL
      DOUBLE PRECISION , DIMENSION(NEL,9),INTENT(IN) :: SAV     
      my_real , DIMENSION(10,NEL),INTENT(OUT) :: JAC_I     
      my_real , DIMENSION(NEL),   INTENT(OUT) :: VOL     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      DOUBLE PRECISION
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),VOLDP 
C     REAL
      my_real
     .   DET(MVSIZ),
     .   PX1(MVSIZ), PX2(MVSIZ), PX3(MVSIZ), PX4(MVSIZ),  
     .   PY1(MVSIZ), PY2(MVSIZ), PY3(MVSIZ), PY4(MVSIZ),  
     .   PZ1(MVSIZ), PZ2(MVSIZ), PZ3(MVSIZ), PZ4(MVSIZ)  
      INTEGER I
      my_real
     .   B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), B4(MVSIZ), 
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ) 
      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,B1DP,C1DP,D1DP
      my_real
     .   D ,PXX, PYY, PZZ, PXY, PYZ, PXZ, GFAC, AA, BB, P, LD
C=======================================================================
      DO I=1,NEL
       X4(I)=ZERO
       Y4(I)=ZERO
       Z4(I)=ZERO
      ENDDO
      DO I=1,NEL
       X1(I)=SAV(I,1)
       Y1(I)=SAV(I,4)
       Z1(I)=SAV(I,7)
       X2(I)=SAV(I,2)
       Y2(I)=SAV(I,5)
       Z2(I)=SAV(I,8)
       X3(I)=SAV(I,3)
       Y3(I)=SAV(I,6)
       Z3(I)=SAV(I,9)
      ENDDO
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
       B1DP  =  Y43*Z42 - Y42*Z43
       B1(I) =  B1DP
       B2(I) =  Y41*Z43 - Y43*Z41
       B3(I) =  Y42*Z41 - Y41*Z42
       B4(I) =  -(B1(I) + B2(I) + B3(I))
C
       C1DP  =  Z43*X42 - Z42*X43
       C1(I) =  C1DP
       C2(I) =  Z41*X43 - Z43*X41
       C3(I) =  Z42*X41 - Z41*X42
       C4(I) =  -(C1(I) + C2(I) + C3(I))
C
       D1DP  =  X43*Y42 - X42*Y43
       D1(I) =  D1DP
       D2(I) =  X41*Y43 - X43*Y41
       D3(I) =  X42*Y41 - X41*Y42
       D4(I) =  -(D1(I) + D2(I) + D3(I))
C
       VOLDP = (X41*B1DP + Y41*C1DP + Z41*D1DP)*ONE_OVER_6
       DET(I) = VOLDP
       VOL(I) = DET(I)
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
      ENDDO
       DO I=1,NEL
        JAC_I(1,I) = PX1(I)
        JAC_I(2,I) = PX2(I)
        JAC_I(3,I) = PX3(I)
        JAC_I(4,I) = PY1(I)
        JAC_I(5,I) = PY2(I)
        JAC_I(6,I) = PY3(I)
        JAC_I(7,I) = PZ1(I)
        JAC_I(8,I) = PZ2(I)
        JAC_I(9,I) = PZ3(I)
        JAC_I(10,I) = VOL(I)
       ENDDO
C-----------
      RETURN
      END
