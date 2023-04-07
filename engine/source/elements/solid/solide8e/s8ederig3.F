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
Chd|  S8EDERIG3                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERIG3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PX5,     PX6,     PX7,     PX8,
     3   PY1,     PY2,     PY3,     PY4,
     4   PY5,     PY6,     PY7,     PY8,
     5   PZ1,     PZ2,     PZ3,     PZ4,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   AJI1,    AJI2,    AJI3,    AJI4,
     8   AJI5,    AJI6,    AJI7,    AJI8,
     9   AJI9,    PR,      PS,      PT,
     A   NEL)
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
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJI1(*), AJI2(*), AJI3(*),
     .   AJI4(*), AJI5(*), AJI6(*),
     .   AJI7(*), AJI8(*), AJI9(*),
     .   PR(8),PS(8),PT(8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C     REAL
C                                                                     12
      my_real
     .   A1PR1,A1PR3,A1PR5,A1PR7,
     .   A4PR1,A4PR3,A4PR5,A4PR7,
     .   A7PR1,A7PR3,A7PR5,A7PR7,
     .   A2PS1,A2PS2,A2PS5,A2PS6,
     .   A5PS1,A5PS2,A5PS5,A5PS6,
     .   A8PS1,A8PS2,A8PS5,A8PS6,
     .   A3PT1,A3PT2,A3PT3,A3PT4,
     .   A6PT1,A6PT2,A6PT3,A6PT4,
     .   A9PT1,A9PT2,A9PT3,A9PT4
C-----------------------------------------------
cc-----pxI
      DO I=1,NEL
        A1PR1=AJI1(I)*PR(1)
        A1PR3=AJI1(I)*PR(3)
        A1PR5=AJI1(I)*PR(5)
        A1PR7=AJI1(I)*PR(7)
        A2PS1=AJI2(I)*PS(1)
        A2PS2=AJI2(I)*PS(2)
        A2PS5=AJI2(I)*PS(5)
        A2PS6=AJI2(I)*PS(6)
        A3PT1=AJI3(I)*PT(1)
        A3PT2=AJI3(I)*PT(2)
        A3PT3=AJI3(I)*PT(3)
        A3PT4=AJI3(I)*PT(4)
       PX1(I)= A1PR1+A2PS1+A3PT1
       PX2(I)=-A1PR1+A2PS2+A3PT2
       PX3(I)= A1PR3-A2PS2+A3PT3
       PX4(I)=-A1PR3-A2PS1+A3PT4
       PX5(I)= A1PR5+A2PS5-A3PT1
       PX6(I)=-A1PR5+A2PS6-A3PT2
       PX7(I)= A1PR7-A2PS6-A3PT3
       PX8(I)=-A1PR7-A2PS5-A3PT4
      ENDDO
C
      DO I=1,NEL
C
        A4PR1=AJI4(I)*PR(1)
        A4PR3=AJI4(I)*PR(3)
        A4PR5=AJI4(I)*PR(5)
        A4PR7=AJI4(I)*PR(7)
        A5PS1=AJI5(I)*PS(1)
        A5PS2=AJI5(I)*PS(2)
        A5PS5=AJI5(I)*PS(5)
        A5PS6=AJI5(I)*PS(6)
        A6PT1=AJI6(I)*PT(1)
        A6PT2=AJI6(I)*PT(2)
        A6PT3=AJI6(I)*PT(3)
        A6PT4=AJI6(I)*PT(4)
       PY1(I)= A4PR1+A5PS1+A6PT1
       PY2(I)=-A4PR1+A5PS2+A6PT2
       PY3(I)= A4PR3-A5PS2+A6PT3
       PY4(I)=-A4PR3-A5PS1+A6PT4
       PY5(I)= A4PR5+A5PS5-A6PT1
       PY6(I)=-A4PR5+A5PS6-A6PT2
       PY7(I)= A4PR7-A5PS6-A6PT3
       PY8(I)=-A4PR7-A5PS5-A6PT4
      ENDDO
C
      DO I=1,NEL
C
        A7PR1=AJI7(I)*PR(1)
        A7PR3=AJI7(I)*PR(3)
        A7PR5=AJI7(I)*PR(5)
        A7PR7=AJI7(I)*PR(7)
        A8PS1=AJI8(I)*PS(1)
        A8PS2=AJI8(I)*PS(2)
        A8PS5=AJI8(I)*PS(5)
        A8PS6=AJI8(I)*PS(6)
        A9PT1=AJI9(I)*PT(1)
        A9PT2=AJI9(I)*PT(2)
        A9PT3=AJI9(I)*PT(3)
        A9PT4=AJI9(I)*PT(4)
       PZ1(I)= A7PR1+A8PS1+A9PT1
       PZ2(I)=-A7PR1+A8PS2+A9PT2
       PZ3(I)= A7PR3-A8PS2+A9PT3
       PZ4(I)=-A7PR3-A8PS1+A9PT4
       PZ5(I)= A7PR5+A8PS5-A9PT1
       PZ6(I)=-A7PR5+A8PS6-A9PT2
       PZ7(I)= A7PR7-A8PS6-A9PT3
       PZ8(I)=-A7PR7-A8PS5-A9PT4
      ENDDO
C
      RETURN
       END
