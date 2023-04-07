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
Chd|  S8EDERIOM3                    source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|====================================================================
      SUBROUTINE S8EDERIOM3(
     1   OFFG,    OFF,     VOL,     NGL,
     2   KSI,     ETA,     ZETA,    WI,
     3   PX1,     PX2,     PX3,     PX4,
     4   PX5,     PX6,     PX7,     PX8,
     5   PY1,     PY2,     PY3,     PY4,
     6   PY5,     PY6,     PY7,     PY8,
     7   PZ1,     PZ2,     PZ3,     PZ4,
     8   PZ5,     PZ6,     PZ7,     PZ8,
     9   PXC1,    PXC2,    PXC3,    PXC4,
     A   PYC1,    PYC2,    PYC3,    PYC4,
     B   PZC1,    PZC2,    PZC3,    PZC4,
     C   BXY1,    BXY2,    BXY3,    BXY4,
     D   BXY5,    BXY6,    BXY7,    BXY8,
     E   BYX1,    BYX2,    BYX3,    BYX4,
     F   BYX5,    BYX6,    BYX7,    BYX8,
     G   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     H   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     I   BZX1,    BZX2,    BZX3,    BZX4,
     J   BZX5,    BZX6,    BZX7,    BZX8,
     K   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     L   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     M   BZY1,    BZY2,    BZY3,    BZY4,
     N   BZY5,    BZY6,    BZY7,    BZY8,
     O   BXX1,    BXX2,    BXX3,    BXX4,
     P   BXX5,    BXX6,    BXX7,    BXX8,
     Q   BYY1,    BYY2,    BYY3,    BYY4,
     R   BYY5,    BYY6,    BYY7,    BYY8,
     S   BZZ1,    BZZ2,    BZZ3,    BZZ4,
     T   BZZ5,    BZZ6,    BZZ7,    BZZ8,
     U   AJI1,    AJI2,    AJI3,    AJI4,
     V   AJI5,    AJI6,    AJI7,    AJI8,
     W   AJI9,    AJ4,     AJ5,     AJ6,
     X   AJ7,     AJ8,     AJ9,     RX,
     Y   RY,      RZ,      SX,      SY,
     Z   SZ,      PR,      PS,      PT,
     1   SMAX,    DELTAX,  ICP,     IDEG,
     2   NU,      VOLP,    NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER ICP,IDEG(*)
C     REAL
      my_real
     .   OFF(*),OFFG(*),VOL(*),KSI,ETA,ZETA,WI,
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   AJI1(*), AJI2(*), AJI3(*),
     .   AJI4(*), AJI5(*), AJI6(*),
     .   AJI7(*), AJI8(*), AJI9(*),
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   RX(*)    ,RY(*)    ,RZ(*)    ,
     .   SX(*)    ,SY(*)    ,SZ(*)    ,
     .   PR(8),PS(8),PT(8),
     .   SMAX(*),DELTAX(*),
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
     .   BXX1(*),BXX2(*),BXX3(*),BXX4(*),
     .   BXX5(*),BXX6(*),BXX7(*),BXX8(*),
     .   BYY1(*),BYY2(*),BYY3(*),BYY4(*),
     .   BYY5(*),BYY6(*),BYY7(*),BYY8(*),
     .   BZZ1(*),BZZ2(*),BZZ3(*),BZZ4(*),
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*),VOLP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I, J ,ICOR
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
     .   A9PT1,A9PT2,A9PT3,A9PT4,
     .   BXHI,BYHI,BZHI,TMP,DETI
C-----------------------------------------------
      DO I=1,NEL
       RX(I) = AJ4(I)
       RY(I) = AJ5(I)
       RZ(I) = AJ6(I)
       SX(I) = AJ7(I)
       SY(I) = AJ8(I)
       SZ(I) = AJ9(I)
      ENDDO
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
      DO I=1,NEL
       OFF(I) = OFFG(I)
       VOL(I) = VOLP(I)
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*VOL(I)*SMAX(I)/WI)
      ENDDO
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
      IF (ICP==11) THEN
       CALL S8BICP11(
     1   PX1,     PX2,     PX3,     PX4,
     2   PX5,     PX6,     PX7,     PX8,
     3   PY1,     PY2,     PY3,     PY4,
     4   PY5,     PY6,     PY7,     PY8,
     5   PZ1,     PZ2,     PZ3,     PZ4,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   PXC1,    PXC2,    PXC3,    PXC4,
     8   PYC1,    PYC2,    PYC3,    PYC4,
     9   PZC1,    PZC2,    PZC3,    PZC4,
     A   BXY1,    BXY2,    BXY3,    BXY4,
     B   BXY5,    BXY6,    BXY7,    BXY8,
     C   BYX1,    BYX2,    BYX3,    BYX4,
     D   BYX5,    BYX6,    BYX7,    BYX8,
     E   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     F   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     G   BZX1,    BZX2,    BZX3,    BZX4,
     H   BZX5,    BZX6,    BZX7,    BZX8,
     I   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     J   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     K   BZY1,    BZY2,    BZY3,    BZY4,
     L   BZY5,    BZY6,    BZY7,    BZY8,
     M   BXX1,    BXX2,    BXX3,    BXX4,
     N   BXX5,    BXX6,    BXX7,    BXX8,
     O   BYY1,    BYY2,    BYY3,    BYY4,
     P   BYY5,    BYY6,    BYY7,    BYY8,
     Q   BZZ1,    BZZ2,    BZZ3,    BZZ4,
     R   BZZ5,    BZZ6,    BZZ7,    BZZ8,
     S   NU,      NEL)
      ENDIF
C
      RETURN
 2000 FORMAT(/' ZERO OR NEGATIVE SUB-VOLUME : DELETE 3D-ELEMENT NB',
     .          I10/)
 2001 FORMAT(/' ZERO OR NEGATIVE SOLID SUB-VOLUME : ELEMENT NB:',
     .          I10/)
      END
