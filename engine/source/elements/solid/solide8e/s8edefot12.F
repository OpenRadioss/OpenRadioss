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
Chd|  S8EDEFOT12                    source/elements/solid/solide8e/s8edefot12.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDEFOT12(
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
     8   R11,     R12,     R13,     R21,
     9   R22,     R23,     R31,     R32,
     A   R33,     DXX,     DXY,     DXZ,
     B   DYX,     DYY,     DYZ,     DZX,
     C   DZY,     DZZ,     ICP,     DET0,
     D   JFAC,    NU,      IDEGE,   OFFG,
     E   ISEL_V,  I_SH,    NEL,     ISMSTR,
     F   JCVT)
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
      INTEGER, INTENT(IN) :: JCVT
      INTEGER ISEL_V,I_SH,ICP,IDEGE(*)
C     REAL
      my_real
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PX5(*), PX6(*), PX7(*), PX8(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PY5(*), PY6(*), PY7(*), PY8(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*), 
     .   PZ5(*), PZ6(*), PZ7(*), PZ8(*),
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
     .   VX1(*), VX2(*), VX3(*), VX4(*), VX5(*), VX6(*), VX7(*), VX8(*),
     .   VY1(*), VY2(*), VY3(*), VY4(*), VY5(*), VY6(*), VY7(*), VY8(*),
     .   VZ1(*), VZ2(*), VZ3(*), VZ4(*), VZ5(*), VZ6(*), VZ7(*), VZ8(*),
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*),
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
     .   BZZ5(*),BZZ6(*),BZZ7(*),BZZ8(*),OFFG(*),DET0(*),JFAC(*),NU(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   VLX1(MVSIZ), VLX2(MVSIZ), VLX3(MVSIZ), VLX4(MVSIZ),
     .   VLX5(MVSIZ), VLX6(MVSIZ), VLX7(MVSIZ), VLX8(MVSIZ),
     .   VLY1(MVSIZ), VLY2(MVSIZ), VLY3(MVSIZ), VLY4(MVSIZ),
     .   VLY5(MVSIZ), VLY6(MVSIZ), VLY7(MVSIZ), VLY8(MVSIZ),
     .   VLZ1(MVSIZ), VLZ2(MVSIZ), VLZ3(MVSIZ), VLZ4(MVSIZ),
     .   VLZ5(MVSIZ), VLZ6(MVSIZ), VLZ7(MVSIZ), VLZ8(MVSIZ)
      my_real
     .   JAC_59_68, JAC_67_49, JAC_48_57,
     .   AJ11, AJ22, AJ33,DET(MVSIZ),FAC,BASE
C-----------------------------------------------
      IF (JCVT > 0 ) THEN
        DO I=1,NEL
               IF(OFFG(I)<=ONE) CYCLE
         VLX1(I)=R11(I)*VX1(I)+R21(I)*VY1(I)+R31(I)*VZ1(I)
         VLY1(I)=R12(I)*VX1(I)+R22(I)*VY1(I)+R32(I)*VZ1(I)
         VLZ1(I)=R13(I)*VX1(I)+R23(I)*VY1(I)+R33(I)*VZ1(I)
         VLX2(I)=R11(I)*VX2(I)+R21(I)*VY2(I)+R31(I)*VZ2(I)
         VLY2(I)=R12(I)*VX2(I)+R22(I)*VY2(I)+R32(I)*VZ2(I)
         VLZ2(I)=R13(I)*VX2(I)+R23(I)*VY2(I)+R33(I)*VZ2(I)
         VLX3(I)=R11(I)*VX3(I)+R21(I)*VY3(I)+R31(I)*VZ3(I)
         VLY3(I)=R12(I)*VX3(I)+R22(I)*VY3(I)+R32(I)*VZ3(I)
         VLZ3(I)=R13(I)*VX3(I)+R23(I)*VY3(I)+R33(I)*VZ3(I)
         VLX4(I)=R11(I)*VX4(I)+R21(I)*VY4(I)+R31(I)*VZ4(I)
         VLY4(I)=R12(I)*VX4(I)+R22(I)*VY4(I)+R32(I)*VZ4(I)
         VLZ4(I)=R13(I)*VX4(I)+R23(I)*VY4(I)+R33(I)*VZ4(I)
         VLX5(I)=R11(I)*VX5(I)+R21(I)*VY5(I)+R31(I)*VZ5(I)
         VLY5(I)=R12(I)*VX5(I)+R22(I)*VY5(I)+R32(I)*VZ5(I)
         VLZ5(I)=R13(I)*VX5(I)+R23(I)*VY5(I)+R33(I)*VZ5(I)
         VLX6(I)=R11(I)*VX6(I)+R21(I)*VY6(I)+R31(I)*VZ6(I)
         VLY6(I)=R12(I)*VX6(I)+R22(I)*VY6(I)+R32(I)*VZ6(I)
         VLZ6(I)=R13(I)*VX6(I)+R23(I)*VY6(I)+R33(I)*VZ6(I)
         VLX7(I)=R11(I)*VX7(I)+R21(I)*VY7(I)+R31(I)*VZ7(I)
         VLY7(I)=R12(I)*VX7(I)+R22(I)*VY7(I)+R32(I)*VZ7(I)
         VLZ7(I)=R13(I)*VX7(I)+R23(I)*VY7(I)+R33(I)*VZ7(I)
         VLX8(I)=R11(I)*VX8(I)+R21(I)*VY8(I)+R31(I)*VZ8(I)
         VLY8(I)=R12(I)*VX8(I)+R22(I)*VY8(I)+R32(I)*VZ8(I)
         VLZ8(I)=R13(I)*VX8(I)+R23(I)*VY8(I)+R33(I)*VZ8(I)
        ENDDO
      ELSE
         VLX1(1:NEL)=VX1(1:NEL)
         VLY1(1:NEL)=VY1(1:NEL)
         VLZ1(1:NEL)=VZ1(1:NEL)
         VLX2(1:NEL)=VX2(1:NEL)
         VLY2(1:NEL)=VY2(1:NEL)
         VLZ2(1:NEL)=VZ2(1:NEL)
         VLX3(1:NEL)=VX3(1:NEL)
         VLY3(1:NEL)=VY3(1:NEL)
         VLZ3(1:NEL)=VZ3(1:NEL)
         VLX4(1:NEL)=VX4(1:NEL)
         VLY4(1:NEL)=VY4(1:NEL)
         VLZ4(1:NEL)=VZ4(1:NEL)
         VLX5(1:NEL)=VX5(1:NEL)
         VLY5(1:NEL)=VY5(1:NEL)
         VLZ5(1:NEL)=VZ5(1:NEL)
         VLX6(1:NEL)=VX6(1:NEL)
         VLY6(1:NEL)=VY6(1:NEL)
         VLZ6(1:NEL)=VZ6(1:NEL)
         VLX7(1:NEL)=VX7(1:NEL)
         VLY7(1:NEL)=VY7(1:NEL)
         VLZ7(1:NEL)=VZ7(1:NEL)
         VLX8(1:NEL)=VX8(1:NEL)
         VLY8(1:NEL)=VY8(1:NEL)
         VLZ8(1:NEL)=VZ8(1:NEL)
      END IF !(JCVT > 0
C   
      IF (I_SH>0) THEN
       DO I=1,NEL
              IF(OFFG(I)<=ONE) CYCLE
        DXY(I) =PXY1(I)*VLX1(I)+PXY2(I)*VLX2(I)
     +         +PXY3(I)*VLX3(I)+PXY4(I)*VLX4(I)
     +         +PXY5(I)*VLX5(I)+PXY6(I)*VLX6(I)
     +         +PXY7(I)*VLX7(I)+PXY8(I)*VLX8(I)
        DXZ(I) =PXZ1(I)*VLX1(I)+PXZ2(I)*VLX2(I)
     +         +PXZ3(I)*VLX3(I)+PXZ4(I)*VLX4(I)
     +         +PXZ5(I)*VLX5(I)+PXZ6(I)*VLX6(I)
     +         +PXZ7(I)*VLX7(I)+PXZ8(I)*VLX8(I)
        DYX(I) =PYX1(I)*VLY1(I)+PYX2(I)*VLY2(I)
     +         +PYX3(I)*VLY3(I)+PYX4(I)*VLY4(I)
     +         +PYX5(I)*VLY5(I)+PYX6(I)*VLY6(I)
     +         +PYX7(I)*VLY7(I)+PYX8(I)*VLY8(I)
        DYZ(I) =PYZ1(I)*VLY1(I)+PYZ2(I)*VLY2(I)
     +         +PYZ3(I)*VLY3(I)+PYZ4(I)*VLY4(I)
     +         +PYZ5(I)*VLY5(I)+PYZ6(I)*VLY6(I)
     +         +PYZ7(I)*VLY7(I)+PYZ8(I)*VLY8(I)
        DZX(I) =PZX1(I)*VLZ1(I)+PZX2(I)*VLZ2(I)
     +         +PZX3(I)*VLZ3(I)+PZX4(I)*VLZ4(I)
     +         +PZX5(I)*VLZ5(I)+PZX6(I)*VLZ6(I)
     +         +PZX7(I)*VLZ7(I)+PZX8(I)*VLZ8(I)
        DZY(I) =PZY1(I)*VLZ1(I)+PZY2(I)*VLZ2(I)
     +         +PZY3(I)*VLZ3(I)+PZY4(I)*VLZ4(I)
     +         +PZY5(I)*VLZ5(I)+PZY6(I)*VLZ6(I)
     +         +PZY7(I)*VLZ7(I)+PZY8(I)*VLZ8(I)
       ENDDO
      ELSE
       DO I=1,NEL
              IF(OFFG(I)<=ONE) CYCLE
        DXY(I) =PY1(I)*VLX1(I)+PY2(I)*VLX2(I)+PY3(I)*VLX3(I)+PY4(I)*VLX4(I)
     +         +PY5(I)*VLX5(I)+PY6(I)*VLX6(I)+PY7(I)*VLX7(I)+PY8(I)*VLX8(I)
        DXZ(I) =PZ1(I)*VLX1(I)+PZ2(I)*VLX2(I)+PZ3(I)*VLX3(I)+PZ4(I)*VLX4(I)
     +         +PZ5(I)*VLX5(I)+PZ6(I)*VLX6(I)+PZ7(I)*VLX7(I)+PZ8(I)*VLX8(I)
        DYX(I) =PX1(I)*VLY1(I)+PX2(I)*VLY2(I)+PX3(I)*VLY3(I)+PX4(I)*VLY4(I)
     +         +PX5(I)*VLY5(I)+PX6(I)*VLY6(I)+PX7(I)*VLY7(I)+PX8(I)*VLY8(I)
        DYZ(I) =PZ1(I)*VLY1(I)+PZ2(I)*VLY2(I)+PZ3(I)*VLY3(I)+PZ4(I)*VLY4(I)
     +         +PZ5(I)*VLY5(I)+PZ6(I)*VLY6(I)+PZ7(I)*VLY7(I)+PZ8(I)*VLY8(I)
        DZX(I) =PX1(I)*VLZ1(I)+PX2(I)*VLZ2(I)+PX3(I)*VLZ3(I)+PX4(I)*VLZ4(I)
     +         +PX5(I)*VLZ5(I)+PX6(I)*VLZ6(I)+PX7(I)*VLZ7(I)+PX8(I)*VLZ8(I)
        DZY(I) =PY1(I)*VLZ1(I)+PY2(I)*VLZ2(I)+PY3(I)*VLZ3(I)+PY4(I)*VLZ4(I)
     +         +PY5(I)*VLZ5(I)+PY6(I)*VLZ6(I)+PY7(I)*VLZ7(I)+PY8(I)*VLZ8(I)
       ENDDO
      END IF !(ISMSTR==11) THEN
C     
       DO I=1,NEL
              IF(OFFG(I)<=ONE) CYCLE
        DXX(I) =PX1(I)*VLX1(I)+PX2(I)*VLX2(I)+PX3(I)*VLX3(I)+PX4(I)*VLX4(I)
     .         +PX5(I)*VLX5(I)+PX6(I)*VLX6(I)+PX7(I)*VLX7(I)+PX8(I)*VLX8(I)
        DYY(I) =PY1(I)*VLY1(I)+PY2(I)*VLY2(I)+PY3(I)*VLY3(I)+PY4(I)*VLY4(I)
     .         +PY5(I)*VLY5(I)+PY6(I)*VLY6(I)+PY7(I)*VLY7(I)+PY8(I)*VLY8(I)
        DZZ(I) =PZ1(I)*VLZ1(I)+PZ2(I)*VLZ2(I)+PZ3(I)*VLZ3(I)+PZ4(I)*VLZ4(I)
     .         +PZ5(I)*VLZ5(I)+PZ6(I)*VLZ6(I)+PZ7(I)*VLZ7(I)+PZ8(I)*VLZ8(I)
       ENDDO
             IF (ISEL_V>0) THEN
#include   "nofusion.inc"
       DO I=1,NEL
              IF(OFFG(I)<=ONE) CYCLE
        DYY(I) =DYY(I)+BXY1(I)*VLX1(I)+BXY2(I)*VLX2(I)
     +                +BXY3(I)*VLX3(I)+BXY4(I)*VLX4(I)
     +                +BXY5(I)*VLX5(I)+BXY6(I)*VLX6(I)
     +                +BXY7(I)*VLX7(I)+BXY8(I)*VLX8(I)
        DZZ(I) =DZZ(I)+BXZ1(I)*VLX1(I)+BXZ2(I)*VLX2(I)
     +                +BXZ3(I)*VLX3(I)+BXZ4(I)*VLX4(I)
     +                +BXZ5(I)*VLX5(I)+BXZ6(I)*VLX6(I)
     +                +BXZ7(I)*VLX7(I)+BXZ8(I)*VLX8(I)
        DXX(I) =DXX(I)+BYX1(I)*VLY1(I)+BYX2(I)*VLY2(I)
     +                +BYX3(I)*VLY3(I)+BYX4(I)*VLY4(I)
     +                +BYX5(I)*VLY5(I)+BYX6(I)*VLY6(I)
     +                +BYX7(I)*VLY7(I)+BYX8(I)*VLY8(I)
        DZZ(I) =DZZ(I)+BYZ1(I)*VLY1(I)+BYZ2(I)*VLY2(I)
     +                +BYZ3(I)*VLY3(I)+BYZ4(I)*VLY4(I)
     +                +BYZ5(I)*VLY5(I)+BYZ6(I)*VLY6(I)
     +                +BYZ7(I)*VLY7(I)+BYZ8(I)*VLY8(I)
        DXX(I) =DXX(I)+BZX1(I)*VLZ1(I)+BZX2(I)*VLZ2(I)
     +                +BZX3(I)*VLZ3(I)+BZX4(I)*VLZ4(I)
     +                +BZX5(I)*VLZ5(I)+BZX6(I)*VLZ6(I)
     +                +BZX7(I)*VLZ7(I)+BZX8(I)*VLZ8(I)
        DYY(I) =DYY(I)+BZY1(I)*VLZ1(I)+BZY2(I)*VLZ2(I)
     +                +BZY3(I)*VLZ3(I)+BZY4(I)*VLZ4(I)
     +                +BZY5(I)*VLZ5(I)+BZY6(I)*VLZ6(I)
     +                +BZY7(I)*VLZ7(I)+BZY8(I)*VLZ8(I)
       ENDDO
             END IF !(ISEL_V>0) THEN
      IF (ICP==1) THEN
        DO I=1,NEL
               IF(OFFG(I)<=ONE) CYCLE
         AJ11=DXX(I)+ONE
         AJ22=DYY(I)+ONE
         AJ33=DZZ(I)+ONE
         JAC_59_68=AJ22*AJ33-DYZ(I)*DZY(I)
         JAC_67_49=DYZ(I)*DZX(I)-DYX(I)*AJ33
         JAC_48_57=DYX(I)*DZY(I)-AJ22*DZX(I)
C       
         DET(I)=AJ11*JAC_59_68+DXY(I)*JAC_67_49+DXZ(I)*JAC_48_57
C-----F'=FAC*F=MF'+[1]-->MF'=FAC*MF+(FAC-ONE)*[1]
         FAC=TWO_THIRD*NU(I)
         BASE = DET0(I)/MAX(EM20,DET(I))
         JFAC(I)=EXP(FAC*LOG(MAX(EM20,BASE)))
        ENDDO
       DO I=1,NEL
        IF(IDEGE(I)>10.OR.OFFG(I)<=ONE) CYCLE
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
