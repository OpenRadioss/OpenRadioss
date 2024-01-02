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
Chd|  S8EDERI_2                     source/elements/solid/solide8e/s8ederi_2.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|-- calls ---------------
Chd|        S8BICP11                      source/elements/solid/solide8e/s8bicp11.F
Chd|====================================================================
      SUBROUTINE S8EDERI_2(
     1   OFFG,    OFF,     VOL,     KSI,
     2   ETA,     ZETA,    WI,      PX1,
     3   PX2,     PX3,     PX4,     PX5,
     4   PX6,     PX7,     PX8,     PY1,
     5   PY2,     PY3,     PY4,     PY5,
     6   PY6,     PY7,     PY8,     PZ1,
     7   PZ2,     PZ3,     PZ4,     PZ5,
     8   PZ6,     PZ7,     PZ8,     PXC1,
     9   PXC2,    PXC3,    PXC4,    PYC1,
     A   PYC2,    PYC3,    PYC4,    PZC1,
     B   PZC2,    PZC3,    PZC4,    BXY1,
     C   BXY2,    BXY3,    BXY4,    BXY5,
     D   BXY6,    BXY7,    BXY8,    BYX1,
     E   BYX2,    BYX3,    BYX4,    BYX5,
     F   BYX6,    BYX7,    BYX8,    BXZ1,
     G   BXZ2,    BXZ3,    BXZ4,    BXZ5,
     H   BXZ6,    BXZ7,    BXZ8,    BZX1,
     I   BZX2,    BZX3,    BZX4,    BZX5,
     J   BZX6,    BZX7,    BZX8,    BYZ1,
     K   BYZ2,    BYZ3,    BYZ4,    BYZ5,
     L   BYZ6,    BYZ7,    BYZ8,    BZY1,
     M   BZY2,    BZY3,    BZY4,    BZY5,
     N   BZY6,    BZY7,    BZY8,    BXX1,
     O   BXX2,    BXX3,    BXX4,    BXX5,
     P   BXX6,    BXX7,    BXX8,    BYY1,
     Q   BYY2,    BYY3,    BYY4,    BYY5,
     R   BYY6,    BYY7,    BYY8,    BZZ1,
     S   BZZ2,    BZZ3,    BZZ4,    BZZ5,
     T   BZZ6,    BZZ7,    BZZ8,    AJ4,
     U   AJ5,     AJ6,     AJ7,     AJ8,
     V   AJ9,     RX,      RY,      RZ,
     W   SX,      SY,      SZ,      SMAX,
     X   DELTAX,  ICP,     IDEG,    NU,
     Y   VOLP,    NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
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
     .   AJ4(*),AJ5(*),AJ6(*),
     .   AJ7(*),AJ8(*),AJ9(*),
     .   RX(*)    ,RY(*)    ,RZ(*)    ,
     .   SX(*)    ,SY(*)    ,SZ(*)    ,
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
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),NU(*)
      DOUBLE PRECISION 
     .   VOLP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J ,ICOR
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
C
      DO I=1,NEL
       OFF(I) = OFFG(I)
       VOL(I) = VOLP(I)
      ENDDO
      IF (IDTS6==0) THEN
       DO I=1,NEL
        DELTAX(I)=MIN(DELTAX(I),HUNDRED28*VOL(I)*SMAX(I)/WI)
       ENDDO
      ELSE
      DO I=1,NEL
       IF (IDEG(I)==0) DELTAX(I)=MIN(DELTAX(I),HUNDRED28*VOL(I)*SMAX(I)/WI)
      ENDDO
      END IF
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
      END
