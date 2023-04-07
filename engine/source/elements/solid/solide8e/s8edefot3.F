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
Chd|  S8EDEFOT3                     source/elements/solid/solide8e/s8edefo3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/s8edefo3.F
Chd|====================================================================
      SUBROUTINE S8EDEFOT3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PX5,     PX6,     PX7,     PX8,
     3   PY1,     PY2,     PY3,     PY4,
     4   PY5,     PY6,     PY7,     PY8,
     5   PZ1,     PZ2,     PZ3,     PZ4,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   BXY1,    BXY2,    BXY3,    BXY4,
     8   BXY5,    BXY6,    BXY7,    BXY8,
     9   BYX1,    BYX2,    BYX3,    BYX4,
     A   BYX5,    BYX6,    BYX7,    BYX8,
     B   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     C   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     D   BZX1,    BZX2,    BZX3,    BZX4,
     E   BZX5,    BZX6,    BZX7,    BZX8,
     F   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     G   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     H   BZY1,    BZY2,    BZY3,    BZY4,
     I   BZY5,    BZY6,    BZY7,    BZY8,
     J   BXX1,    BXX2,    BXX3,    BXX4,
     K   BXX5,    BXX6,    BXX7,    BXX8,
     L   BYY1,    BYY2,    BYY3,    BYY4,
     M   BYY5,    BYY6,    BYY7,    BYY8,
     N   BZZ1,    BZZ2,    BZZ3,    BZZ4,
     O   BZZ5,    BZZ6,    BZZ7,    BZZ8,
     P   PXY1,    PXY2,    PXY3,    PXY4,
     Q   PXY5,    PXY6,    PXY7,    PXY8,
     R   PYX1,    PYX2,    PYX3,    PYX4,
     S   PYX5,    PYX6,    PYX7,    PYX8,
     T   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     U   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     V   PZX1,    PZX2,    PZX3,    PZX4,
     W   PZX5,    PZX6,    PZX7,    PZX8,
     X   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     Y   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     Z   PZY1,    PZY2,    PZY3,    PZY4,
     1   PZY5,    PZY6,    PZY7,    PZY8,
     2   VX1,     VX2,     VX3,     VX4,
     3   VX5,     VX6,     VX7,     VX8,
     4   VY1,     VY2,     VY3,     VY4,
     5   VY5,     VY6,     VY7,     VY8,
     6   VZ1,     VZ2,     VZ3,     VZ4,
     7   VZ5,     VZ6,     VZ7,     VZ8,
     8   DXX,     DXY,     DXZ,     DYX,
     9   DYY,     DYZ,     DZX,     DZY,
     A   DZZ,     ICP,     DET0,    JFAC,
     B   NU,      I_SH,    IDEGE,   DXX_A,
     C   DYY_A,   DZZ_A,   OFFG,    NEL,
     D   ISMSTR)
C                                                                     12
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
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER ICP,I_SH,IDEGE(*)
C     REAL
      my_real
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*),DXX_A(*),DYY_A(*),DZZ_A(*),
     .   PXY1(MVSIZ),PXY2(MVSIZ),PXY3(MVSIZ),PXY4(MVSIZ),
     .   PXY5(MVSIZ),PXY6(MVSIZ),PXY7(MVSIZ),PXY8(MVSIZ),
     .   PYX1(MVSIZ),PYX2(MVSIZ),PYX3(MVSIZ),PYX4(MVSIZ),
     .   PYX5(MVSIZ),PYX6(MVSIZ),PYX7(MVSIZ),PYX8(MVSIZ),
     .   PXZ1(MVSIZ),PXZ2(MVSIZ),PXZ3(MVSIZ),PXZ4(MVSIZ),
     .   PXZ5(MVSIZ),PXZ6(MVSIZ),PXZ7(MVSIZ),PXZ8(MVSIZ),
     .   PZX1(MVSIZ),PZX2(MVSIZ),PZX3(MVSIZ),PZX4(MVSIZ),
     .   PZX5(MVSIZ),PZX6(MVSIZ),PZX7(MVSIZ),PZX8(MVSIZ),
     .   PYZ1(MVSIZ),PYZ2(MVSIZ),PYZ3(MVSIZ),PYZ4(MVSIZ),
     .   PYZ5(MVSIZ),PYZ6(MVSIZ),PYZ7(MVSIZ),PYZ8(MVSIZ),
     .   PZY1(MVSIZ),PZY2(MVSIZ),PZY3(MVSIZ),PZY4(MVSIZ),
     .   PZY5(MVSIZ),PZY6(MVSIZ),PZY7(MVSIZ),PZY8(MVSIZ),
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
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),DET0(*),JFAC(*),NU(*),OFFG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DET(MVSIZ),FAC,BASE
C-----------------------------------------------
C---- this sub_routine has been called 2 times, one for Ismstr=10,12 another for 11
      IF (ISMSTR==11.AND.I_SH>0) THEN
       DO I=1,NEL
       DXY(I) =PXY1(I)*VX1(I)+PXY2(I)*VX2(I)
     +        +PXY3(I)*VX3(I)+PXY4(I)*VX4(I)
     +        +PXY5(I)*VX5(I)+PXY6(I)*VX6(I)
     +        +PXY7(I)*VX7(I)+PXY8(I)*VX8(I)
       DXZ(I) =PXZ1(I)*VX1(I)+PXZ2(I)*VX2(I)
     +        +PXZ3(I)*VX3(I)+PXZ4(I)*VX4(I)
     +        +PXZ5(I)*VX5(I)+PXZ6(I)*VX6(I)
     +        +PXZ7(I)*VX7(I)+PXZ8(I)*VX8(I)
       DYX(I) =PYX1(I)*VY1(I)+PYX2(I)*VY2(I)
     +        +PYX3(I)*VY3(I)+PYX4(I)*VY4(I)
     +        +PYX5(I)*VY5(I)+PYX6(I)*VY6(I)
     +        +PYX7(I)*VY7(I)+PYX8(I)*VY8(I)
       DYZ(I) =PYZ1(I)*VY1(I)+PYZ2(I)*VY2(I)
     +        +PYZ3(I)*VY3(I)+PYZ4(I)*VY4(I)
     +        +PYZ5(I)*VY5(I)+PYZ6(I)*VY6(I)
     +        +PYZ7(I)*VY7(I)+PYZ8(I)*VY8(I)
       DZX(I) =PZX1(I)*VZ1(I)+PZX2(I)*VZ2(I)
     +        +PZX3(I)*VZ3(I)+PZX4(I)*VZ4(I)
     +        +PZX5(I)*VZ5(I)+PZX6(I)*VZ6(I)
     +        +PZX7(I)*VZ7(I)+PZX8(I)*VZ8(I)
       DZY(I) =PZY1(I)*VZ1(I)+PZY2(I)*VZ2(I)
     +        +PZY3(I)*VZ3(I)+PZY4(I)*VZ4(I)
     +        +PZY5(I)*VZ5(I)+PZY6(I)*VZ6(I)
     +        +PZY7(I)*VZ7(I)+PZY8(I)*VZ8(I)
       ENDDO
      ELSE
       DO I=1,NEL
	    IF (ISMSTR==12.AND.OFFG(I)>ONE) CYCLE
       DXY(I) =PY1(I)*VX1(I)+PY2(I)*VX2(I)+PY3(I)*VX3(I)+PY4(I)*VX4(I)
     +        +PY5(I)*VX5(I)+PY6(I)*VX6(I)+PY7(I)*VX7(I)+PY8(I)*VX8(I)
       DXZ(I) =PZ1(I)*VX1(I)+PZ2(I)*VX2(I)+PZ3(I)*VX3(I)+PZ4(I)*VX4(I)
     +        +PZ5(I)*VX5(I)+PZ6(I)*VX6(I)+PZ7(I)*VX7(I)+PZ8(I)*VX8(I)
       DYX(I) =PX1(I)*VY1(I)+PX2(I)*VY2(I)+PX3(I)*VY3(I)+PX4(I)*VY4(I)
     +        +PX5(I)*VY5(I)+PX6(I)*VY6(I)+PX7(I)*VY7(I)+PX8(I)*VY8(I)
       DYZ(I) =PZ1(I)*VY1(I)+PZ2(I)*VY2(I)+PZ3(I)*VY3(I)+PZ4(I)*VY4(I)
     +        +PZ5(I)*VY5(I)+PZ6(I)*VY6(I)+PZ7(I)*VY7(I)+PZ8(I)*VY8(I)
       DZX(I) =PX1(I)*VZ1(I)+PX2(I)*VZ2(I)+PX3(I)*VZ3(I)+PX4(I)*VZ4(I)
     +        +PX5(I)*VZ5(I)+PX6(I)*VZ6(I)+PX7(I)*VZ7(I)+PX8(I)*VZ8(I)
       DZY(I) =PY1(I)*VZ1(I)+PY2(I)*VZ2(I)+PY3(I)*VZ3(I)+PY4(I)*VZ4(I)
     +        +PY5(I)*VZ5(I)+PY6(I)*VZ6(I)+PY7(I)*VZ7(I)+PY8(I)*VZ8(I)
       ENDDO
      END IF !(ISMSTR==11) THEN
C
      IF (ICP==11) THEN
#include   "nofusion.inc"
       DO I=1,NEL
        DXX(I) =BXX1(I)*VX1(I)+BXX2(I)*VX2(I)+BXX3(I)*VX3(I)+
     .          BXX4(I)*VX4(I)+BXX5(I)*VX5(I)+BXX6(I)*VX6(I)+
     .          BXX7(I)*VX7(I)+BXX8(I)*VX8(I)
        DYY(I) =BYY1(I)*VY1(I)+BYY2(I)*VY2(I)+BYY3(I)*VY3(I)+
     .          BYY4(I)*VY4(I)+BYY5(I)*VY5(I)+BYY6(I)*VY6(I)+
     .          BYY7(I)*VY7(I)+BYY8(I)*VY8(I)
        DZZ(I) =BZZ1(I)*VZ1(I)+BZZ2(I)*VZ2(I)+BZZ3(I)*VZ3(I)+
     .          BZZ4(I)*VZ4(I)+BZZ5(I)*VZ5(I)+BZZ6(I)*VZ6(I)+
     .          BZZ7(I)*VZ7(I)+BZZ8(I)*VZ8(I)
        DYY(I) =DYY(I)+BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +        +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +        +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +        +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +        +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +        +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +        +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +        +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +        +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +        +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +        +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +        +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +        +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +        +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +        +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +        +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +        +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +        +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +        +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
       ENDDO
      ELSE
       DO I=1,NEL
	    IF (ISMSTR==12.AND.OFFG(I)>ONE) CYCLE
       DXX(I) =PX1(I)*VX1(I)+PX2(I)*VX2(I)+PX3(I)*VX3(I)+PX4(I)*VX4(I)
     .        +PX5(I)*VX5(I)+PX6(I)*VX6(I)+PX7(I)*VX7(I)+PX8(I)*VX8(I)
       DYY(I) =PY1(I)*VY1(I)+PY2(I)*VY2(I)+PY3(I)*VY3(I)+PY4(I)*VY4(I)
     .        +PY5(I)*VY5(I)+PY6(I)*VY6(I)+PY7(I)*VY7(I)+PY8(I)*VY8(I)
       DZZ(I) =PZ1(I)*VZ1(I)+PZ2(I)*VZ2(I)+PZ3(I)*VZ3(I)+PZ4(I)*VZ4(I)
     .        +PZ5(I)*VZ5(I)+PZ6(I)*VZ6(I)+PZ7(I)*VZ7(I)+PZ8(I)*VZ8(I)
       ENDDO
      END IF !(ICP==11) THEN
C   
      IF (I_SH>1) THEN
       IF (ISMSTR==11) THEN
#include   "nofusion.inc"
       DO I=1,NEL
        DYY(I) =DYY(I)+BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +        +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +        +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +        +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +        +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +        +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +        +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +        +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +        +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +        +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +        +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +        +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +        +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +        +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +        +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +        +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +        +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +        +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +        +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
       ENDDO
       ELSE
         DO I=1,NEL
           DYY_A(I) =BXY1(I)*VX1(I)+BXY2(I)*VX2(I)
     +              +BXY3(I)*VX3(I)+BXY4(I)*VX4(I)
     +              +BXY5(I)*VX5(I)+BXY6(I)*VX6(I)
     +              +BXY7(I)*VX7(I)+BXY8(I)*VX8(I)
     +              +BZY1(I)*VZ1(I)+BZY2(I)*VZ2(I)
     +              +BZY3(I)*VZ3(I)+BZY4(I)*VZ4(I)
     +              +BZY5(I)*VZ5(I)+BZY6(I)*VZ6(I)
     +              +BZY7(I)*VZ7(I)+BZY8(I)*VZ8(I)
           DZZ_A(I) =BXZ1(I)*VX1(I)+BXZ2(I)*VX2(I)
     +              +BXZ3(I)*VX3(I)+BXZ4(I)*VX4(I)
     +              +BXZ5(I)*VX5(I)+BXZ6(I)*VX6(I)
     +              +BXZ7(I)*VX7(I)+BXZ8(I)*VX8(I)
     +              +BYZ1(I)*VY1(I)+BYZ2(I)*VY2(I)
     +              +BYZ3(I)*VY3(I)+BYZ4(I)*VY4(I)
     +              +BYZ5(I)*VY5(I)+BYZ6(I)*VY6(I)
     +              +BYZ7(I)*VY7(I)+BYZ8(I)*VY8(I)
           DXX_A(I) =BYX1(I)*VY1(I)+BYX2(I)*VY2(I)
     +              +BYX3(I)*VY3(I)+BYX4(I)*VY4(I)
     +              +BYX5(I)*VY5(I)+BYX6(I)*VY6(I)
     +              +BYX7(I)*VY7(I)+BYX8(I)*VY8(I)
     +              +BZX1(I)*VZ1(I)+BZX2(I)*VZ2(I)
     +              +BZX3(I)*VZ3(I)+BZX4(I)*VZ4(I)
     +              +BZX5(I)*VZ5(I)+BZX6(I)*VZ6(I)
     +              +BZX7(I)*VZ7(I)+BZX8(I)*VZ8(I)
         ENDDO
       END IF !(ISMSTR==11) THEN
      END IF !(I_SH>1) THEN
C      
      IF (ICP==1) THEN
       CALL JACOB_J33(
     1   DET,     DXX,     DXY,     DXZ,
     2   DYX,     DYY,     DYZ,     DZX,
     3   DZY,     DZZ,     NEL)
C-----F'=FAC*F=MF'+[1]-->MF'=FAC*MF+(FAC-ONE)*[1]
        DO I=1,NEL
         FAC=TWO_THIRD*NU(I)
         BASE = DET0(I)/MAX(EM20,DET(I))
         IF(ISMSTR==12.AND.OFFG(I)>ONE) BASE=ONE
         JFAC(I)=EXP(FAC*LOG(MAX(EM20,BASE)))
c         JFAC(I)=(DET0(I)/MAX(EM20,DET(I)))**FAC
        ENDDO
       DO I=1,NEL
        IF(IDEGE(I)>10.OR.(ISMSTR==12.AND.OFFG(I)>ONE)) CYCLE
        DXX(I) = JFAC(I)*DXX(I)+JFAC(I)-ONE
        DYY(I) = JFAC(I)*DYY(I)+JFAC(I)-ONE
        DZZ(I) = JFAC(I)*DZZ(I)+JFAC(I)-ONE
        DXY(I) = JFAC(I)*DXY(I)
        DYX(I) = JFAC(I)*DYX(I)
        DZX(I) = JFAC(I)*DZX(I)
        DXZ(I) = JFAC(I)*DXZ(I)
        DYZ(I) = JFAC(I)*DYZ(I)
        DZY(I) = JFAC(I)*DZY(I)
       ENDDO
      END IF !(ICP==1) THEN
C
      RETURN
      END
