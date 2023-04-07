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
Chd|  S6PROJ3                       source/elements/thickshell/solide6c/s6rcoor3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S6RCOOR3                      source/elements/thickshell/solide6c/s6rcoor3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6PROJ3(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      Y1,      Y2,
     3   Y3,      Y4,      Y5,      Y6,
     4   Z1,      Z2,      Z3,      Z4,
     5   Z5,      Z6,      VX1,     VX2,
     6   VX3,     VX4,     VX5,     VX6,
     7   VY1,     VY2,     VY3,     VY4,
     8   VY5,     VY6,     VZ1,     VZ2,
     9   VZ3,     VZ4,     VZ5,     VZ6,
     A   DI,      NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), 
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), 
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), 
     .  VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), 
     .  VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), 
     .  VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), 
     .  DI(MVSIZ,6)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   XVY,XVZ,YVX,YVZ,ZVX,ZVY, 
     .   AR(3),ALR(3)
C-----------------------------------------------
      DO I=1,NEL
        XVY= X1(I)*VY1(I)+X2(I)*VY2(I)+X3(I)*VY3(I)
     .      +X4(I)*VY4(I)+X5(I)*VY5(I)+X6(I)*VY6(I)
        XVZ= X1(I)*VZ1(I)+X2(I)*VZ2(I)+X3(I)*VZ3(I)
     .      +X4(I)*VZ4(I)+X5(I)*VZ5(I)+X6(I)*VZ6(I)
        YVX= Y1(I)*VX1(I)+Y2(I)*VX2(I)+Y3(I)*VX3(I)
     .      +Y4(I)*VX4(I)+Y5(I)*VX5(I)+Y6(I)*VX6(I)
        YVZ= Y1(I)*VZ1(I)+Y2(I)*VZ2(I)+Y3(I)*VZ3(I)
     .      +Y4(I)*VZ4(I)+Y5(I)*VZ5(I)+Y6(I)*VZ6(I)
        ZVY= Z1(I)*VY1(I)+Z2(I)*VY2(I)+Z3(I)*VY3(I)
     .      +Z4(I)*VY4(I)+Z5(I)*VY5(I)+Z6(I)*VY6(I)
        ZVX= Z1(I)*VX1(I)+Z2(I)*VX2(I)+Z3(I)*VX3(I)
     .      +Z4(I)*VX4(I)+Z5(I)*VX5(I)+Z6(I)*VX6(I)
        AR(1)=-ZVY+YVZ
        AR(2)= ZVX-XVZ
        AR(3)=-YVX+XVY
        ALR(1) =DI(I,1)*AR(1)+DI(I,4)*AR(2)+DI(I,5)*AR(3)
        ALR(2) =DI(I,4)*AR(1)+DI(I,2)*AR(2)+DI(I,6)*AR(3)
        ALR(3) =DI(I,5)*AR(1)+DI(I,6)*AR(2)+DI(I,3)*AR(3)
C
        VX1(I) = VX1(I)-(Z1(I)*ALR(2)-Y1(I)*ALR(3))
        VY1(I) = VY1(I)-(X1(I)*ALR(3)-Z1(I)*ALR(1))
        VZ1(I) = VZ1(I)-(Y1(I)*ALR(1)-X1(I)*ALR(2))
        VX2(I) = VX2(I)-(Z2(I)*ALR(2)-Y2(I)*ALR(3))
        VY2(I) = VY2(I)-(X2(I)*ALR(3)-Z2(I)*ALR(1))
        VZ2(I) = VZ2(I)-(Y2(I)*ALR(1)-X2(I)*ALR(2))
        VX3(I) = VX3(I)-(Z3(I)*ALR(2)-Y3(I)*ALR(3))
        VY3(I) = VY3(I)-(X3(I)*ALR(3)-Z3(I)*ALR(1))
        VZ3(I) = VZ3(I)-(Y3(I)*ALR(1)-X3(I)*ALR(2))
        VX4(I) = VX4(I)-(Z4(I)*ALR(2)-Y4(I)*ALR(3))
        VY4(I) = VY4(I)-(X4(I)*ALR(3)-Z4(I)*ALR(1))
        VZ4(I) = VZ4(I)-(Y4(I)*ALR(1)-X4(I)*ALR(2))
        VX5(I) = VX5(I)-(Z5(I)*ALR(2)-Y5(I)*ALR(3))
        VY5(I) = VY5(I)-(X5(I)*ALR(3)-Z5(I)*ALR(1))
        VZ5(I) = VZ5(I)-(Y5(I)*ALR(1)-X5(I)*ALR(2))
        VX6(I) = VX6(I)-(Z6(I)*ALR(2)-Y6(I)*ALR(3))
        VY6(I) = VY6(I)-(X6(I)*ALR(3)-Z6(I)*ALR(1))
        VZ6(I) = VZ6(I)-(Y6(I)*ALR(1)-X6(I)*ALR(2))
C
      ENDDO
      RETURN
      END
