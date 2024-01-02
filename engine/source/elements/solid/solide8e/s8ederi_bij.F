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
Chd|  S8EDERI_BIJ                   source/elements/solid/solide8e/s8ederi_bij.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERI_BIJ(
     1   PX1,     PX2,     PX3,     PX4,
     2   PX5,     PX6,     PX7,     PX8,
     3   PY1,     PY2,     PY3,     PY4,
     4   PY5,     PY6,     PY7,     PY8,
     5   PZ1,     PZ2,     PZ3,     PZ4,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   PXC1,    PXC2,    PXC3,    PXC4,
     8   PYC1,    PYC2,    PYC3,    PYC4,
     9   PZC1,    PZC2,    PZC3,    PZC4,
     A   PXY1,    PXY2,    PXY3,    PXY4,
     B   PXY5,    PXY6,    PXY7,    PXY8,
     C   PYX1,    PYX2,    PYX3,    PYX4,
     D   PYX5,    PYX6,    PYX7,    PYX8,
     E   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     F   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     G   PZX1,    PZX2,    PZX3,    PZX4,
     H   PZX5,    PZX6,    PZX7,    PZX8,
     I   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     J   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     K   PZY1,    PZY2,    PZY3,    PZY4,
     L   PZY5,    PZY6,    PZY7,    PZY8,
     M   BXY1,    BXY2,    BXY3,    BXY4,
     N   BXY5,    BXY6,    BXY7,    BXY8,
     O   BYX1,    BYX2,    BYX3,    BYX4,
     P   BYX5,    BYX6,    BYX7,    BYX8,
     Q   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     R   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     S   BZX1,    BZX2,    BZX3,    BZX4,
     T   BZX5,    BZX6,    BZX7,    BZX8,
     U   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     V   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     W   BZY1,    BZY2,    BZY3,    BZY4,
     X   BZY5,    BZY6,    BZY7,    BZY8,
     Y   NU,      NU1,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   PXY1(*),PXY2(*),PXY3(*),PXY4(*),
     .   PXY5(*),PXY6(*),PXY7(*),PXY8(*),
     .   PYX1(*),PYX2(*),PYX3(*),PYX4(*),
     .   PYX5(*),PYX6(*),PYX7(*),PYX8(*),
     .   PXZ1(*),PXZ2(*),PXZ3(*),PXZ4(*),
     .   PXZ5(*),PXZ6(*),PXZ7(*),PXZ8(*),
     .   PZX1(*),PZX2(*),PZX3(*),PZX4(*),
     .   PZX5(*),PZX6(*),PZX7(*),PZX8(*),
     .   PYZ1(*),PYZ2(*),PYZ3(*),PYZ4(*),
     .   PYZ5(*),PYZ6(*),PYZ7(*),PYZ8(*),
     .   PZY1(*),PZY2(*),PZY3(*),PZY4(*),
     .   PZY5(*),PZY6(*),PZY7(*),PZY8(*),
     .   PXC1(*), PXC2(*), PXC3(*), PXC4(*),
     .   PYC1(*), PYC2(*), PYC3(*), PYC4(*),
     .   PZC1(*), PZC2(*), PZC3(*), PZC4(*),
     .   BXY1(*),BXY2(*),BXY3(*),BXY4(*),
     .   BXY5(*),BXY6(*),BXY7(*),BXY8(*),
     .   BYX1(*),BYX2(*),BYX3(*),BYX4(*),
     .   BYX5(*),BYX6(*),BYX7(*),BYX8(*),
     .   BXZ1(*),BXZ2(*),BXZ3(*),BXZ4(*),
     .   BXZ5(*),BXZ6(*),BXZ7(*),BXZ8(*),
     .   BZX1(*),BZX2(*),BZX3(*),BZX4(*),
     .   BZX5(*),BZX6(*),BZX7(*),BZX8(*),
     .   BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .   BYZ5(*),BYZ6(*),BYZ7(*),BYZ8(*),
     .   BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .   BZY5(*),BZY6(*),BZY7(*),BZY8(*),
     .   NU(*)  ,NU1(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   X1,X3,Y1,Y2,Z2,Z3,X24,Y34,Z14
C-----------------------------------------------
C---PX_I=X1234_I+PXC_I; PYX_I=X23_I+PXC_I; PZX_I=X12_I+PXC_I
C-----X2_I=0->X1_I = X12_I ; X3_I = X23_I X24_I =X1234_I-X1_I-X3_I
C-----verifie valeur de X4_I otherwise to be calculated
C---PY_I=Y1234_I+PYC_I; PXY_I=Y23_I+PYC_I; PZY_I=Y13_I+PYC_I
C-----Y3_I=0->Y1_I = Y13_I ; Y2_I = Y23_I Y34_I =Y1234_I-Y1_I-Y2_I
C---PZ_I=Z1234_I+PZC_I; PXZ_I=Z12_I+PZC_I; PYZ_I=Z13_I+PZC_I
C-----Z1_I=0->Z2_I = Z12_I ; Z3_I = Z13_I Z14_I =Z1234_I-Z2_I-Z3_I
C-------|0 BYX BZX |
C-------|BXY 0 BZY |  same than H8A
C-------|BXZ BYZ 0 |
C-BXY_I=-nu1*X1_I-nu*X24_I
C-BXZ_I=-nu1*X3_I-nu*X24_I
C-BYX_I=-nu1*Y1_I-nu*Y34_I
C-BYZ_I=-nu1*Y2_I-nu*Y34_I
C-BZX_I=-nu1*Z2_I-nu*Z14_I
C-BZY_I=-nu1*Z3_I-nu*Z14_I
      DO I=1,NEL
       X1 = PZX1(I)-PXC1(I)
       X3 = PYX1(I)-PXC1(I)
       Y1 = PZY1(I)-PYC1(I)
       Y2 = PXY1(I)-PYC1(I)
       Z2 = PXZ1(I)-PZC1(I)
       Z3 = PYZ1(I)-PZC1(I)
       X24= NU(I)*(PX1(I)-PXC1(I)-X1-X3)
       Y34= NU(I)*(PY1(I)-PYC1(I)-Y1-Y2)
       Z14= NU(I)*(PZ1(I)-PZC1(I)-Z2-Z3)
       BXY1(I)=-NU1(I)*X1-X24
       BXZ1(I)=-NU1(I)*X3-X24
       BYX1(I)=-NU1(I)*Y1-Y34
       BYZ1(I)=-NU1(I)*Y2-Y34
       BZX1(I)=-NU1(I)*Z3-Z14
       BZY1(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=1,NEL
       X1 = PZX2(I)-PXC2(I)
       X3 = PYX2(I)-PXC2(I)
       Y1 = PZY2(I)-PYC2(I)
       Y2 = PXY2(I)-PYC2(I)
       Z2 = PXZ2(I)-PZC2(I)
       Z3 = PYZ2(I)-PZC2(I)
       X24= NU(I)*(PX2(I)-PXC2(I)-X1-X3)
       Y34= NU(I)*(PY2(I)-PYC2(I)-Y1-Y2)
       Z14= NU(I)*(PZ2(I)-PZC2(I)-Z2-Z3)
       BXY2(I)=-NU1(I)*X1-X24
       BXZ2(I)=-NU1(I)*X3-X24
       BYX2(I)=-NU1(I)*Y1-Y34
       BYZ2(I)=-NU1(I)*Y2-Y34
       BZX2(I)=-NU1(I)*Z3-Z14
       BZY2(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=1,NEL
       X1 = PZX3(I)-PXC3(I)
       X3 = PYX3(I)-PXC3(I)
       Y1 = PZY3(I)-PYC3(I)
       Y2 = PXY3(I)-PYC3(I)
       Z2 = PXZ3(I)-PZC3(I)
       Z3 = PYZ3(I)-PZC3(I)
       X24= NU(I)*(PX3(I)-PXC3(I)-X1-X3)
       Y34= NU(I)*(PY3(I)-PYC3(I)-Y1-Y2)
       Z14= NU(I)*(PZ3(I)-PZC3(I)-Z2-Z3)
       BXY3(I)=-NU1(I)*X1-X24
       BXZ3(I)=-NU1(I)*X3-X24
       BYX3(I)=-NU1(I)*Y1-Y34
       BYZ3(I)=-NU1(I)*Y2-Y34
       BZX3(I)=-NU1(I)*Z3-Z14
       BZY3(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=1,NEL
       X1 = PZX4(I)-PXC4(I)
       X3 = PYX4(I)-PXC4(I)
       Y1 = PZY4(I)-PYC4(I)
       Y2 = PXY4(I)-PYC4(I)
       Z2 = PXZ4(I)-PZC4(I)
       Z3 = PYZ4(I)-PZC4(I)
       X24= NU(I)*(PX4(I)-PXC4(I)-X1-X3)
       Y34= NU(I)*(PY4(I)-PYC4(I)-Y1-Y2)
       Z14= NU(I)*(PZ4(I)-PZC4(I)-Z2-Z3)
       BXY4(I)=-NU1(I)*X1-X24
       BXZ4(I)=-NU1(I)*X3-X24
       BYX4(I)=-NU1(I)*Y1-Y34
       BYZ4(I)=-NU1(I)*Y2-Y34
       BZX4(I)=-NU1(I)*Z3-Z14
       BZY4(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=1,NEL
       X1 = PZX5(I)+PXC3(I)
       X3 = PYX5(I)+PXC3(I)
       Y1 = PZY5(I)+PYC3(I)
       Y2 = PXY5(I)+PYC3(I)
       Z2 = PXZ5(I)+PZC3(I)
       Z3 = PYZ5(I)+PZC3(I)
       X24= NU(I)*(PX5(I)+PXC3(I)-X1-X3)
       Y34= NU(I)*(PY5(I)+PYC3(I)-Y1-Y2)
       Z14= NU(I)*(PZ5(I)+PZC3(I)-Z2-Z3)
       BXY5(I)=-NU1(I)*X1-X24
       BXZ5(I)=-NU1(I)*X3-X24
       BYX5(I)=-NU1(I)*Y1-Y34
       BYZ5(I)=-NU1(I)*Y2-Y34
       BZX5(I)=-NU1(I)*Z3-Z14
       BZY5(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=1,NEL
       X1 = PZX6(I)+PXC4(I)
       X3 = PYX6(I)+PXC4(I)
       Y1 = PZY6(I)+PYC4(I)
       Y2 = PXY6(I)+PYC4(I)
       Z2 = PXZ6(I)+PZC4(I)
       Z3 = PYZ6(I)+PZC4(I)
       X24= NU(I)*(PX6(I)+PXC4(I)-X1-X3)
       Y34= NU(I)*(PY6(I)+PYC4(I)-Y1-Y2)
       Z14= NU(I)*(PZ6(I)+PZC4(I)-Z2-Z3)
       BXY6(I)=-NU1(I)*X1-X24
       BXZ6(I)=-NU1(I)*X3-X24
       BYX6(I)=-NU1(I)*Y1-Y34
       BYZ6(I)=-NU1(I)*Y2-Y34
       BZX6(I)=-NU1(I)*Z3-Z14
       BZY6(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=1,NEL
       X1 = PZX7(I)+PXC1(I)
       X3 = PYX7(I)+PXC1(I)
       Y1 = PZY7(I)+PYC1(I)
       Y2 = PXY7(I)+PYC1(I)
       Z2 = PXZ7(I)+PZC1(I)
       Z3 = PYZ7(I)+PZC1(I)
       X24= NU(I)*(PX7(I)+PXC1(I)-X1-X3)
       Y34= NU(I)*(PY7(I)+PYC1(I)-Y1-Y2)
       Z14= NU(I)*(PZ7(I)+PZC1(I)-Z2-Z3)
       BXY7(I)=-NU1(I)*X1-X24
       BXZ7(I)=-NU1(I)*X3-X24
       BYX7(I)=-NU1(I)*Y1-Y34
       BYZ7(I)=-NU1(I)*Y2-Y34
       BZX7(I)=-NU1(I)*Z3-Z14
       BZY7(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      DO I=1,NEL
       X1 = PZX8(I)+PXC2(I)
       X3 = PYX8(I)+PXC2(I)
       Y1 = PZY8(I)+PYC2(I)
       Y2 = PXY8(I)+PYC2(I)
       Z2 = PXZ8(I)+PZC2(I)
       Z3 = PYZ8(I)+PZC2(I)
       X24= NU(I)*(PX8(I)+PXC2(I)-X1-X3)
       Y34= NU(I)*(PY8(I)+PYC2(I)-Y1-Y2)
       Z14= NU(I)*(PZ8(I)+PZC2(I)-Z2-Z3)
       BXY8(I)=-NU1(I)*X1-X24
       BXZ8(I)=-NU1(I)*X3-X24
       BYX8(I)=-NU1(I)*Y1-Y34
       BYZ8(I)=-NU1(I)*Y2-Y34
       BZX8(I)=-NU1(I)*Z3-Z14
       BZY8(I)=-NU1(I)*Z2-Z14
      ENDDO
C      
      RETURN
      END
