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
Chd|  S8ESELECSH                    source/elements/solid/solide8e/s8eselecsh.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8ESELECSH(
     1   NPTR,    NPTS,    NPTT,    NNPT,
     2   PX1,     PX2,     PX3,     PX4,
     3   PX5,     PX6,     PX7,     PX8,
     4   PY1,     PY2,     PY3,     PY4,
     5   PY5,     PY6,     PY7,     PY8,
     6   PZ1,     PZ2,     PZ3,     PZ4,
     7   PZ5,     PZ6,     PZ7,     PZ8,
     8   P0XY1,   P0XY2,   P0XY3,   P0XY4,
     9   P0XY5,   P0XY6,   P0XY7,   P0XY8,
     A   P0YX1,   P0YX2,   P0YX3,   P0YX4,
     B   P0YX5,   P0YX6,   P0YX7,   P0YX8,
     C   P0XZ1,   P0XZ2,   P0XZ3,   P0XZ4,
     D   P0XZ5,   P0XZ6,   P0XZ7,   P0XZ8,
     E   P0ZX1,   P0ZX2,   P0ZX3,   P0ZX4,
     F   P0ZX5,   P0ZX6,   P0ZX7,   P0ZX8,
     G   P0YZ1,   P0YZ2,   P0YZ3,   P0YZ4,
     H   P0YZ5,   P0YZ6,   P0YZ7,   P0YZ8,
     I   P0ZY1,   P0ZY2,   P0ZY3,   P0ZY4,
     J   P0ZY5,   P0ZY6,   P0ZY7,   P0ZY8,
     K   NEL)
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
      INTEGER NPTR,NPTS,NPTT,NNPT 
C     REAL
      my_real
     .  PX1(MVSIZ,NNPT),PX2(MVSIZ,NNPT),PX3(MVSIZ,NNPT),PX4(MVSIZ,NNPT),
     .  PX5(MVSIZ,NNPT),PX6(MVSIZ,NNPT),PX7(MVSIZ,NNPT),PX8(MVSIZ,NNPT),
     .  PY1(MVSIZ,NNPT),PY2(MVSIZ,NNPT),PY3(MVSIZ,NNPT),PY4(MVSIZ,NNPT),
     .  PY5(MVSIZ,NNPT),PY6(MVSIZ,NNPT),PY7(MVSIZ,NNPT),PY8(MVSIZ,NNPT),
     .  PZ1(MVSIZ,NNPT),PZ2(MVSIZ,NNPT),PZ3(MVSIZ,NNPT),PZ4(MVSIZ,NNPT),
     .  PZ5(MVSIZ,NNPT),PZ6(MVSIZ,NNPT),PZ7(MVSIZ,NNPT),PZ8(MVSIZ,NNPT),
     .   P0XY1(MVSIZ,2),P0XY2(MVSIZ,2),P0XY3(MVSIZ,2),P0XY4(MVSIZ,2),
     .   P0XY5(MVSIZ,2),P0XY6(MVSIZ,2),P0XY7(MVSIZ,2),P0XY8(MVSIZ,2),
     .   P0YX1(MVSIZ,2),P0YX2(MVSIZ,2),P0YX3(MVSIZ,2),P0YX4(MVSIZ,2),
     .   P0YX5(MVSIZ,2),P0YX6(MVSIZ,2),P0YX7(MVSIZ,2),P0YX8(MVSIZ,2),
     .   P0XZ1(MVSIZ,2),P0XZ2(MVSIZ,2),P0XZ3(MVSIZ,2),P0XZ4(MVSIZ,2),
     .   P0XZ5(MVSIZ,2),P0XZ6(MVSIZ,2),P0XZ7(MVSIZ,2),P0XZ8(MVSIZ,2),
     .   P0ZX1(MVSIZ,2),P0ZX2(MVSIZ,2),P0ZX3(MVSIZ,2),P0ZX4(MVSIZ,2),
     .   P0ZX5(MVSIZ,2),P0ZX6(MVSIZ,2),P0ZX7(MVSIZ,2),P0ZX8(MVSIZ,2),
     .   P0YZ1(MVSIZ,2),P0YZ2(MVSIZ,2),P0YZ3(MVSIZ,2),P0YZ4(MVSIZ,2),
     .   P0YZ5(MVSIZ,2),P0YZ6(MVSIZ,2),P0YZ7(MVSIZ,2),P0YZ8(MVSIZ,2),
     .   P0ZY1(MVSIZ,2),P0ZY2(MVSIZ,2),P0ZY3(MVSIZ,2),P0ZY4(MVSIZ,2),
     .   P0ZY5(MVSIZ,2),P0ZY6(MVSIZ,2),P0ZY7(MVSIZ,2),P0ZY8(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,IR,IS,IT,IP,I1,I2,I3,I4
      INTEGER IXY(4,2),IXZ(4,2),IYZ(4,2)
      DATA IXY/1,5,3,7,2,6,4,8/
      DATA IXZ/1,3,2,4,5,7,6,8/
      DATA IYZ/1,5,2,6,3,7,4,8/
C------ moyen in 3 directions
      DO IR=1,NPTR
        I1 = IXY(1,IR)
        I2 = IXY(2,IR)
        I3 = IXY(3,IR)
        I4 = IXY(4,IR)
       DO I=1,NEL
       P0YX1(I,IR)=FOURTH*(PX1(I,I1)+PX1(I,I2)+PX1(I,I3)+PX1(I,I4))
       P0YX2(I,IR)=FOURTH*(PX2(I,I1)+PX2(I,I2)+PX2(I,I3)+PX2(I,I4))
       P0YX3(I,IR)=FOURTH*(PX3(I,I1)+PX3(I,I2)+PX3(I,I3)+PX3(I,I4))
       P0YX4(I,IR)=FOURTH*(PX4(I,I1)+PX4(I,I2)+PX4(I,I3)+PX4(I,I4))
       P0YX5(I,IR)=FOURTH*(PX5(I,I1)+PX5(I,I2)+PX5(I,I3)+PX5(I,I4))
       P0YX6(I,IR)=FOURTH*(PX6(I,I1)+PX6(I,I2)+PX6(I,I3)+PX6(I,I4))
       P0YX7(I,IR)=FOURTH*(PX7(I,I1)+PX7(I,I2)+PX7(I,I3)+PX7(I,I4))
       P0YX8(I,IR)=FOURTH*(PX8(I,I1)+PX8(I,I2)+PX8(I,I3)+PX8(I,I4))
       ENDDO
       DO I=1,NEL
       P0XY1(I,IR)=FOURTH*(PY1(I,I1)+PY1(I,I2)+PY1(I,I3)+PY1(I,I4))
       P0XY2(I,IR)=FOURTH*(PY2(I,I1)+PY2(I,I2)+PY2(I,I3)+PY2(I,I4))
       P0XY3(I,IR)=FOURTH*(PY3(I,I1)+PY3(I,I2)+PY3(I,I3)+PY3(I,I4))
       P0XY4(I,IR)=FOURTH*(PY4(I,I1)+PY4(I,I2)+PY4(I,I3)+PY4(I,I4))
       P0XY5(I,IR)=FOURTH*(PY5(I,I1)+PY5(I,I2)+PY5(I,I3)+PY5(I,I4))
       P0XY6(I,IR)=FOURTH*(PY6(I,I1)+PY6(I,I2)+PY6(I,I3)+PY6(I,I4))
       P0XY7(I,IR)=FOURTH*(PY7(I,I1)+PY7(I,I2)+PY7(I,I3)+PY7(I,I4))
       P0XY8(I,IR)=FOURTH*(PY8(I,I1)+PY8(I,I2)+PY8(I,I3)+PY8(I,I4))
       ENDDO
      ENDDO
C      
      DO IT=1,NPTT
        I1 = IXZ(1,IT)
        I2 = IXZ(2,IT)
        I3 = IXZ(3,IT)
        I4 = IXZ(4,IT)
       DO I=1,NEL
       P0ZX1(I,IT)=FOURTH*(PX1(I,I1)+PX1(I,I2)+PX1(I,I3)+PX1(I,I4))
       P0ZX2(I,IT)=FOURTH*(PX2(I,I1)+PX2(I,I2)+PX2(I,I3)+PX2(I,I4))
       P0ZX3(I,IT)=FOURTH*(PX3(I,I1)+PX3(I,I2)+PX3(I,I3)+PX3(I,I4))
       P0ZX4(I,IT)=FOURTH*(PX4(I,I1)+PX4(I,I2)+PX4(I,I3)+PX4(I,I4))
       P0ZX5(I,IT)=FOURTH*(PX5(I,I1)+PX5(I,I2)+PX5(I,I3)+PX5(I,I4))
       P0ZX6(I,IT)=FOURTH*(PX6(I,I1)+PX6(I,I2)+PX6(I,I3)+PX6(I,I4))
       P0ZX7(I,IT)=FOURTH*(PX7(I,I1)+PX7(I,I2)+PX7(I,I3)+PX7(I,I4))
       P0ZX8(I,IT)=FOURTH*(PX8(I,I1)+PX8(I,I2)+PX8(I,I3)+PX8(I,I4))
       ENDDO
       DO I=1,NEL
       P0XZ1(I,IT)=FOURTH*(PZ1(I,I1)+PZ1(I,I2)+PZ1(I,I3)+PZ1(I,I4))
       P0XZ2(I,IT)=FOURTH*(PZ2(I,I1)+PZ2(I,I2)+PZ2(I,I3)+PZ2(I,I4))
       P0XZ3(I,IT)=FOURTH*(PZ3(I,I1)+PZ3(I,I2)+PZ3(I,I3)+PZ3(I,I4))
       P0XZ4(I,IT)=FOURTH*(PZ4(I,I1)+PZ4(I,I2)+PZ4(I,I3)+PZ4(I,I4))
       P0XZ5(I,IT)=FOURTH*(PZ5(I,I1)+PZ5(I,I2)+PZ5(I,I3)+PZ5(I,I4))
       P0XZ6(I,IT)=FOURTH*(PZ6(I,I1)+PZ6(I,I2)+PZ6(I,I3)+PZ6(I,I4))
       P0XZ7(I,IT)=FOURTH*(PZ7(I,I1)+PZ7(I,I2)+PZ7(I,I3)+PZ7(I,I4))
       P0XZ8(I,IT)=FOURTH*(PZ8(I,I1)+PZ8(I,I2)+PZ8(I,I3)+PZ8(I,I4))
       ENDDO
      ENDDO
C     
      DO IS=1,NPTS
        I1 = IYZ(1,IS)
        I2 = IYZ(2,IS)
        I3 = IYZ(3,IS)
        I4 = IYZ(4,IS)
       DO I=1,NEL
       P0ZY1(I,IS)=FOURTH*(PY1(I,I1)+PY1(I,I2)+PY1(I,I3)+PY1(I,I4))
       P0ZY2(I,IS)=FOURTH*(PY2(I,I1)+PY2(I,I2)+PY2(I,I3)+PY2(I,I4))
       P0ZY3(I,IS)=FOURTH*(PY3(I,I1)+PY3(I,I2)+PY3(I,I3)+PY3(I,I4))
       P0ZY4(I,IS)=FOURTH*(PY4(I,I1)+PY4(I,I2)+PY4(I,I3)+PY4(I,I4))
       P0ZY5(I,IS)=FOURTH*(PY5(I,I1)+PY5(I,I2)+PY5(I,I3)+PY5(I,I4))
       P0ZY6(I,IS)=FOURTH*(PY6(I,I1)+PY6(I,I2)+PY6(I,I3)+PY6(I,I4))
       P0ZY7(I,IS)=FOURTH*(PY7(I,I1)+PY7(I,I2)+PY7(I,I3)+PY7(I,I4))
       P0ZY8(I,IS)=FOURTH*(PY8(I,I1)+PY8(I,I2)+PY8(I,I3)+PY8(I,I4))
       ENDDO
       DO I=1,NEL
       P0YZ1(I,IS)=FOURTH*(PZ1(I,I1)+PZ1(I,I2)+PZ1(I,I3)+PZ1(I,I4))
       P0YZ2(I,IS)=FOURTH*(PZ2(I,I1)+PZ2(I,I2)+PZ2(I,I3)+PZ2(I,I4))
       P0YZ3(I,IS)=FOURTH*(PZ3(I,I1)+PZ3(I,I2)+PZ3(I,I3)+PZ3(I,I4))
       P0YZ4(I,IS)=FOURTH*(PZ4(I,I1)+PZ4(I,I2)+PZ4(I,I3)+PZ4(I,I4))
       P0YZ5(I,IS)=FOURTH*(PZ5(I,I1)+PZ5(I,I2)+PZ5(I,I3)+PZ5(I,I4))
       P0YZ6(I,IS)=FOURTH*(PZ6(I,I1)+PZ6(I,I2)+PZ6(I,I3)+PZ6(I,I4))
       P0YZ7(I,IS)=FOURTH*(PZ7(I,I1)+PZ7(I,I2)+PZ7(I,I3)+PZ7(I,I4))
       P0YZ8(I,IS)=FOURTH*(PZ8(I,I1)+PZ8(I,I2)+PZ8(I,I3)+PZ8(I,I4))
       ENDDO
      ENDDO
C
      RETURN
       END
