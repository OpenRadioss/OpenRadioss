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
Chd|  S8ZDEFOT3                     source/elements/solid/solide8z/s8zdefot3.F
Chd|-- called by -----------
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        JACOB_J33                     source/elements/solid/solide8e/jacob_j33.F
Chd|====================================================================
      SUBROUTINE S8ZDEFOT3(
     1   PX1,     PX2,     PX3,     PX4,
     2   PX5,     PX6,     PX7,     PX8,
     3   PY1,     PY2,     PY3,     PY4,
     4   PY5,     PY6,     PY7,     PY8,
     5   PZ1,     PZ2,     PZ3,     PZ4,
     6   PZ5,     PZ6,     PZ7,     PZ8,
     7   PXY1,    PXY2,    PXY3,    PXY4,
     8   PXY5,    PXY6,    PXY7,    PXY8,
     9   PYX1,    PYX2,    PYX3,    PYX4,
     A   PYX5,    PYX6,    PYX7,    PYX8,
     B   PXZ1,    PXZ2,    PXZ3,    PXZ4,
     C   PXZ5,    PXZ6,    PXZ7,    PXZ8,
     D   PZX1,    PZX2,    PZX3,    PZX4,
     E   PZX5,    PZX6,    PZX7,    PZX8,
     F   PYZ1,    PYZ2,    PYZ3,    PYZ4,
     G   PYZ5,    PYZ6,    PYZ7,    PYZ8,
     H   PZY1,    PZY2,    PZY3,    PZY4,
     I   PZY5,    PZY6,    PZY7,    PZY8,
     J   BXY1,    BXY2,    BXY3,    BXY4,
     K   BXY5,    BXY6,    BXY7,    BXY8,
     L   BYX1,    BYX2,    BYX3,    BYX4,
     M   BYX5,    BYX6,    BYX7,    BYX8,
     N   BXZ1,    BXZ2,    BXZ3,    BXZ4,
     O   BXZ5,    BXZ6,    BXZ7,    BXZ8,
     P   BZX1,    BZX2,    BZX3,    BZX4,
     Q   BZX5,    BZX6,    BZX7,    BZX8,
     R   BYZ1,    BYZ2,    BYZ3,    BYZ4,
     S   BYZ5,    BYZ6,    BYZ7,    BYZ8,
     T   BZY1,    BZY2,    BZY3,    BZY4,
     U   BZY5,    BZY6,    BZY7,    BZY8,
     V   VX1,     VX2,     VX3,     VX4,
     W   VX5,     VX6,     VX7,     VX8,
     X   VY1,     VY2,     VY3,     VY4,
     Y   VY5,     VY6,     VY7,     VY8,
     Z   VZ1,     VZ2,     VZ3,     VZ4,
     1   VZ5,     VZ6,     VZ7,     VZ8,
     2   DXX,     DXY,     DXZ,     DYX,
     3   DYY,     DYZ,     DZX,     DZY,
     4   DZZ,     ICP,     DET0,    JFAC,
     5   NU,      IDEG,    NEL)
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
      INTEGER ICP,IDEG(*)
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
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*) ,DET0(*),JFAC(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   FAC,DET(MVSIZ),BASE
C-----------------------------------------------
       DXX(1:NEL) =PX1(1:NEL)*VX1(1:NEL)+PX2(1:NEL)*VX2(1:NEL)+PX3(1:NEL)*VX3(1:NEL)+PX4(1:NEL)*VX4(1:NEL)
     .        +PX5(1:NEL)*VX5(1:NEL)+PX6(1:NEL)*VX6(1:NEL)+PX7(1:NEL)*VX7(1:NEL)+PX8(1:NEL)*VX8(1:NEL)
       DYY(1:NEL) =PY1(1:NEL)*VY1(1:NEL)+PY2(1:NEL)*VY2(1:NEL)+PY3(1:NEL)*VY3(1:NEL)+PY4(1:NEL)*VY4(1:NEL)
     .        +PY5(1:NEL)*VY5(1:NEL)+PY6(1:NEL)*VY6(1:NEL)+PY7(1:NEL)*VY7(1:NEL)+PY8(1:NEL)*VY8(1:NEL)
       DZZ(1:NEL) =PZ1(1:NEL)*VZ1(1:NEL)+PZ2(1:NEL)*VZ2(1:NEL)+PZ3(1:NEL)*VZ3(1:NEL)+PZ4(1:NEL)*VZ4(1:NEL)
     .        +PZ5(1:NEL)*VZ5(1:NEL)+PZ6(1:NEL)*VZ6(1:NEL)+PZ7(1:NEL)*VZ7(1:NEL)+PZ8(1:NEL)*VZ8(1:NEL)
       DXY(1:NEL) =PXY1(1:NEL)*VX1(1:NEL)+PXY2(1:NEL)*VX2(1:NEL)
     +        +PXY3(1:NEL)*VX3(1:NEL)+PXY4(1:NEL)*VX4(1:NEL)
     +        +PXY5(1:NEL)*VX5(1:NEL)+PXY6(1:NEL)*VX6(1:NEL)
     +        +PXY7(1:NEL)*VX7(1:NEL)+PXY8(1:NEL)*VX8(1:NEL)
       DXZ(1:NEL) =PXZ1(1:NEL)*VX1(1:NEL)+PXZ2(1:NEL)*VX2(1:NEL)
     +        +PXZ3(1:NEL)*VX3(1:NEL)+PXZ4(1:NEL)*VX4(1:NEL)
     +        +PXZ5(1:NEL)*VX5(1:NEL)+PXZ6(1:NEL)*VX6(1:NEL)
     +        +PXZ7(1:NEL)*VX7(1:NEL)+PXZ8(1:NEL)*VX8(1:NEL)
       DYX(1:NEL) =PYX1(1:NEL)*VY1(1:NEL)+PYX2(1:NEL)*VY2(1:NEL)
     +        +PYX3(1:NEL)*VY3(1:NEL)+PYX4(1:NEL)*VY4(1:NEL)
     +        +PYX5(1:NEL)*VY5(1:NEL)+PYX6(1:NEL)*VY6(1:NEL)
     +        +PYX7(1:NEL)*VY7(1:NEL)+PYX8(1:NEL)*VY8(1:NEL)
       DYZ(1:NEL) =PYZ1(1:NEL)*VY1(1:NEL)+PYZ2(1:NEL)*VY2(1:NEL)
     +        +PYZ3(1:NEL)*VY3(1:NEL)+PYZ4(1:NEL)*VY4(1:NEL)
     +        +PYZ5(1:NEL)*VY5(1:NEL)+PYZ6(1:NEL)*VY6(1:NEL)
     +        +PYZ7(1:NEL)*VY7(1:NEL)+PYZ8(1:NEL)*VY8(1:NEL)
       DZX(1:NEL) =PZX1(1:NEL)*VZ1(1:NEL)+PZX2(1:NEL)*VZ2(1:NEL)
     +        +PZX3(1:NEL)*VZ3(1:NEL)+PZX4(1:NEL)*VZ4(1:NEL)
     +        +PZX5(1:NEL)*VZ5(1:NEL)+PZX6(1:NEL)*VZ6(1:NEL)
     +        +PZX7(1:NEL)*VZ7(1:NEL)+PZX8(1:NEL)*VZ8(1:NEL)
       DZY(1:NEL) =PZY1(1:NEL)*VZ1(1:NEL)+PZY2(1:NEL)*VZ2(1:NEL)
     +        +PZY3(1:NEL)*VZ3(1:NEL)+PZY4(1:NEL)*VZ4(1:NEL)
     +        +PZY5(1:NEL)*VZ5(1:NEL)+PZY6(1:NEL)*VZ6(1:NEL)
     +        +PZY7(1:NEL)*VZ7(1:NEL)+PZY8(1:NEL)*VZ8(1:NEL)
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
         JFAC(I)=EXP(FAC*LOG(MAX(EM20,BASE)))
         IF (IDEG(I)>10) JFAC(I) =ONE 
        ENDDO
        DXX(1:NEL) = JFAC(1:NEL)*DXX(1:NEL)+JFAC(1:NEL)-ONE
        DYY(1:NEL) = JFAC(1:NEL)*DYY(1:NEL)+JFAC(1:NEL)-ONE
        DZZ(1:NEL) = JFAC(1:NEL)*DZZ(1:NEL)+JFAC(1:NEL)-ONE
        DXY(1:NEL) = JFAC(1:NEL)*DXY(1:NEL)
        DYX(1:NEL) = JFAC(1:NEL)*DYX(1:NEL)
        DZX(1:NEL) = JFAC(1:NEL)*DZX(1:NEL)
        DXZ(1:NEL) = JFAC(1:NEL)*DXZ(1:NEL)
        DYZ(1:NEL) = JFAC(1:NEL)*DYZ(1:NEL)
        DZY(1:NEL) = JFAC(1:NEL)*DZY(1:NEL)
      ELSE
        DYY(1:NEL) =DYY(1:NEL)+BXY1(1:NEL)*VX1(1:NEL)+BXY2(1:NEL)*VX2(1:NEL)
     +        +BXY3(1:NEL)*VX3(1:NEL)+BXY4(1:NEL)*VX4(1:NEL)
     +        +BXY5(1:NEL)*VX5(1:NEL)+BXY6(1:NEL)*VX6(1:NEL)
     +        +BXY7(1:NEL)*VX7(1:NEL)+BXY8(1:NEL)*VX8(1:NEL)
        DZZ(1:NEL) =DZZ(1:NEL)+BXZ1(1:NEL)*VX1(1:NEL)+BXZ2(1:NEL)*VX2(1:NEL)
     +        +BXZ3(1:NEL)*VX3(1:NEL)+BXZ4(1:NEL)*VX4(1:NEL)
     +        +BXZ5(1:NEL)*VX5(1:NEL)+BXZ6(1:NEL)*VX6(1:NEL)
     +        +BXZ7(1:NEL)*VX7(1:NEL)+BXZ8(1:NEL)*VX8(1:NEL)
        DXX(1:NEL) =DXX(1:NEL)+BYX1(1:NEL)*VY1(1:NEL)+BYX2(1:NEL)*VY2(1:NEL)
     +        +BYX3(1:NEL)*VY3(1:NEL)+BYX4(1:NEL)*VY4(1:NEL)
     +        +BYX5(1:NEL)*VY5(1:NEL)+BYX6(1:NEL)*VY6(1:NEL)
     +        +BYX7(1:NEL)*VY7(1:NEL)+BYX8(1:NEL)*VY8(1:NEL)
        DZZ(1:NEL) =DZZ(1:NEL)+BYZ1(1:NEL)*VY1(1:NEL)+BYZ2(1:NEL)*VY2(1:NEL)
     +        +BYZ3(1:NEL)*VY3(1:NEL)+BYZ4(1:NEL)*VY4(1:NEL)
     +        +BYZ5(1:NEL)*VY5(1:NEL)+BYZ6(1:NEL)*VY6(1:NEL)
     +        +BYZ7(1:NEL)*VY7(1:NEL)+BYZ8(1:NEL)*VY8(1:NEL)
        DXX(1:NEL) =DXX(1:NEL)+BZX1(1:NEL)*VZ1(1:NEL)+BZX2(1:NEL)*VZ2(1:NEL)
     +        +BZX3(1:NEL)*VZ3(1:NEL)+BZX4(1:NEL)*VZ4(1:NEL)
     +        +BZX5(1:NEL)*VZ5(1:NEL)+BZX6(1:NEL)*VZ6(1:NEL)
     +        +BZX7(1:NEL)*VZ7(1:NEL)+BZX8(1:NEL)*VZ8(1:NEL)
        DYY(1:NEL) =DYY(1:NEL)+BZY1(1:NEL)*VZ1(1:NEL)+BZY2(1:NEL)*VZ2(1:NEL)
     +        +BZY3(1:NEL)*VZ3(1:NEL)+BZY4(1:NEL)*VZ4(1:NEL)
     +        +BZY5(1:NEL)*VZ5(1:NEL)+BZY6(1:NEL)*VZ6(1:NEL)
     +        +BZY7(1:NEL)*VZ7(1:NEL)+BZY8(1:NEL)*VZ8(1:NEL)
      ENDIF !IF (ICP==1) THEN
C
      RETURN
      END
