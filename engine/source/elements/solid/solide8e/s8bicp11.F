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
Chd|  S8BICP11                      source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EDERI3                      source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERIO3                     source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERIOM3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERIOT3                    source/elements/solid/solide8e/s8ederi3.F
Chd|        S8EDERI_2                     source/elements/solid/solide8e/s8ederi3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8BICP11(
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
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J 
C     REAL
C                                                                     12
      my_real
     .   BXHI,BYHI,BZHI,TEMP
C-----------for the imcompressible laws
C-------|BXX BYX BZX |
C-------|BXY BYY BZY |  same than H8A
C-------|BXZ BYZ BZZ |
        TEMP=HALF
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX1(I)-PXC1(I))
        BYHI=TEMP*(PY1(I)-PYC1(I))
        BZHI=TEMP*(PZ1(I)-PZC1(I))
c        PX1(I)=PXC1(I)+TWO*BXHI
c        PY1(I)=PYC1(I)+TWO*BYHI
c        PZ1(I)=PZC1(I)+TWO*BZHI
        BXY1(I)=-BXHI
        BXZ1(I)=-BXHI
        BYX1(I)=-BYHI
        BYZ1(I)=-BYHI
        BZX1(I)=-BZHI
        BZY1(I)=-BZHI
       ENDDO
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX2(I)-PXC2(I))
        BYHI=TEMP*(PY2(I)-PYC2(I))
        BZHI=TEMP*(PZ2(I)-PZC2(I))
c        PX2(I)=PXC2(I)+TWO*BXHI
c        PY2(I)=PYC2(I)+TWO*BYHI
c        PZ2(I)=PZC2(I)+TWO*BZHI
        BXY2(I)=-BXHI
        BXZ2(I)=-BXHI
        BYX2(I)=-BYHI
        BYZ2(I)=-BYHI
        BZX2(I)=-BZHI
        BZY2(I)=-BZHI
       ENDDO
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX3(I)-PXC3(I))
        BYHI=TEMP*(PY3(I)-PYC3(I))
        BZHI=TEMP*(PZ3(I)-PZC3(I))
c        PX3(I)=PXC3(I)+TWO*BXHI
c        PY3(I)=PYC3(I)+TWO*BYHI
c        PZ3(I)=PZC3(I)+TWO*BZHI
        BXY3(I)=-BXHI
        BXZ3(I)=-BXHI
        BYX3(I)=-BYHI
        BYZ3(I)=-BYHI
        BZX3(I)=-BZHI
        BZY3(I)=-BZHI
       ENDDO
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX4(I)-PXC4(I))
        BYHI=TEMP*(PY4(I)-PYC4(I))
        BZHI=TEMP*(PZ4(I)-PZC4(I))
c        PX4(I)=PXC4(I)+TWO*BXHI
c        PY4(I)=PYC4(I)+TWO*BYHI
c        PZ4(I)=PZC4(I)+TWO*BZHI
        BXY4(I)=-BXHI
        BXZ4(I)=-BXHI
        BYX4(I)=-BYHI
        BYZ4(I)=-BYHI
        BZX4(I)=-BZHI
        BZY4(I)=-BZHI
       ENDDO
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX5(I)+PXC3(I))
        BYHI=TEMP*(PY5(I)+PYC3(I))
        BZHI=TEMP*(PZ5(I)+PZC3(I))
c        PX5(I)=-PXC3(I)+TWO*BXHI
c        PY5(I)=-PYC3(I)+TWO*BYHI
c        PZ5(I)=-PZC3(I)+TWO*BZHI
        BXY5(I)=-BXHI
        BXZ5(I)=-BXHI
        BYX5(I)=-BYHI
        BYZ5(I)=-BYHI
        BZX5(I)=-BZHI
        BZY5(I)=-BZHI
       ENDDO
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX6(I)+PXC4(I))
        BYHI=TEMP*(PY6(I)+PYC4(I))
        BZHI=TEMP*(PZ6(I)+PZC4(I))
c        PX6(I)=-PXC4(I)+TWO*BXHI
c        PY6(I)=-PYC4(I)+TWO*BYHI
c        PZ6(I)=-PZC4(I)+TWO*BZHI
        BXY6(I)=-BXHI
        BXZ6(I)=-BXHI
        BYX6(I)=-BYHI
        BYZ6(I)=-BYHI
        BZX6(I)=-BZHI
        BZY6(I)=-BZHI
       ENDDO
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX7(I)+PXC1(I))
        BYHI=TEMP*(PY7(I)+PYC1(I))
        BZHI=TEMP*(PZ7(I)+PZC1(I))
c        PX7(I)=-PXC1(I)+TWO*BXHI
c        PY7(I)=-PYC1(I)+TWO*BYHI
c        PZ7(I)=-PZC1(I)+TWO*BZHI
        BXY7(I)=-BXHI
        BXZ7(I)=-BXHI
        BYX7(I)=-BYHI
        BYZ7(I)=-BYHI
        BZX7(I)=-BZHI
        BZY7(I)=-BZHI
       ENDDO
       DO I=1,NEL
c        TEMP=NU(I)
        BXHI=TEMP*(PX8(I)+PXC2(I))
        BYHI=TEMP*(PY8(I)+PYC2(I))
        BZHI=TEMP*(PZ8(I)+PZC2(I))
c        PX8(I)=-PXC2(I)+TWO*BXHI
c        PY8(I)=-PYC2(I)+TWO*BYHI
c        PZ8(I)=-PZC2(I)+TWO*BZHI
        BXY8(I)=-BXHI
        BXZ8(I)=-BXHI
        BYX8(I)=-BYHI
        BYZ8(I)=-BYHI
        BZX8(I)=-BZHI
        BZY8(I)=-BZHI
       ENDDO
       DO I=1,NEL
        BXX1(I) = PX1(I)
        BYY1(I) = PY1(I)
        BZZ1(I) = PZ1(I)
        BXX2(I) = PX2(I)
        BYY2(I) = PY2(I)
        BZZ2(I) = PZ2(I)
        BXX3(I) = PX3(I)
        BYY3(I) = PY3(I)
        BZZ3(I) = PZ3(I)
        BXX4(I) = PX4(I)
        BYY4(I) = PY4(I)
        BZZ4(I) = PZ4(I)
        BXX5(I) = PX5(I)
        BYY5(I) = PY5(I)
        BZZ5(I) = PZ5(I)
        BXX6(I) = PX6(I)
        BYY6(I) = PY6(I)
        BZZ6(I) = PZ6(I)
        BXX7(I) = PX7(I)
        BYY7(I) = PY7(I)
        BZZ7(I) = PZ7(I)
        BXX8(I) = PX8(I)
        BYY8(I) = PY8(I)
        BZZ8(I) = PZ8(I)
       ENDDO
C
      RETURN
      END 
