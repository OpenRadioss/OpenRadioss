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
Chd|  S4JAC_I                       source/elements/solid/solide4/s4jac_i.F
Chd|-- called by -----------
Chd|        S4UPD11T12                    source/elements/solid/solide4/s4upd11t12.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE S4JAC_I(
     .              X1   ,X2   ,X3   ,X4   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,
     .              JAC_I,NEL  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
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
      INTEGER NEL
      my_real
     .   JAC_I(10,*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*) 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), B4(MVSIZ), 
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ),VOL(MVSIZ)
      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,B1DP,C1DP,D1DP,VOLDP
      my_real
     .   D ,PX1,PX2,PX3,PX4,PY1,PY2,PY3,PY4,PZ1,PZ2,PZ3,PZ4
C=======================================================================
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
       VOL(I) = VOLDP
      ENDDO
C
      DO I=1,NEL
        D = ONE/VOL(I)/SIX
        PX1=-B1(I)*D
        PY1=-C1(I)*D
        PZ1=-D1(I)*D
        PX2=-B2(I)*D
        PY2=-C2(I)*D
        PZ2=-D2(I)*D
        PX3=-B3(I)*D
        PY3=-C3(I)*D
        PZ3=-D3(I)*D
        PX4=-B4(I)*D
        PY4=-C4(I)*D
        PZ4=-D4(I)*D
        JAC_I(1,I) = PX1
        JAC_I(2,I) = PX2
        JAC_I(3,I) = PX3
        JAC_I(4,I) = PY1
        JAC_I(5,I) = PY2
        JAC_I(6,I) = PY3
        JAC_I(7,I) = PZ1
        JAC_I(8,I) = PZ2
        JAC_I(9,I) = PZ3
        JAC_I(10,I) = VOL(I)
      ENDDO
C-----------
      RETURN
      END
