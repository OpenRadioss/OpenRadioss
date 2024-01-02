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
Chd|  S8EGETBIJ                     source/elements/solid/solide8e/s8egetbij.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EGETBIJ(
     .    BXY1, BXY2, BXY3, BXY4, BXY5, BXY6, BXY7, BXY8,
     .    BYX1, BYX2, BYX3, BYX4, BYX5, BYX6, BYX7, BYX8,
     .    BXZ1, BXZ2, BXZ3, BXZ4, BXZ5, BXZ6, BXZ7, BXZ8,
     .    BZX1, BZX2, BZX3, BZX4, BZX5, BZX6, BZX7, BZX8,
     .    BYZ1, BYZ2, BYZ3, BYZ4, BYZ5, BYZ6, BYZ7, BYZ8,
     .    BZY1, BZY2, BZY3, BZY4, BZY5, BZY6, BZY7, BZY8,
     .    PIJ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
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
     .   PIJ(NEL,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K
C-----------------------------------------------
       DO I=1,NEL
C       
        BXY1(I) = PIJ(I,25) 
        BYX1(I) = PIJ(I,26) 
        BXZ1(I) = PIJ(I,27) 
        BZX1(I) = PIJ(I,28) 
        BYZ1(I) = PIJ(I,29) 
        BZY1(I) = PIJ(I,30) 
C                
        BXY2(I) = PIJ(I,31) 
        BYX2(I) = PIJ(I,32) 
        BXZ2(I) = PIJ(I,33) 
        BZX2(I) = PIJ(I,34) 
        BYZ2(I) = PIJ(I,35) 
        BZY2(I) = PIJ(I,36) 
C                
        BXY3(I) = PIJ(I,37) 
        BYX3(I) = PIJ(I,38) 
        BXZ3(I) = PIJ(I,39) 
        BZX3(I) = PIJ(I,40) 
        BYZ3(I) = PIJ(I,41) 
        BZY3(I) = PIJ(I,42) 
C                
        BXY4(I) = PIJ(I,43) 
        BYX4(I) = PIJ(I,44) 
        BXZ4(I) = PIJ(I,45) 
        BZX4(I) = PIJ(I,46) 
        BYZ4(I) = PIJ(I,47) 
        BZY4(I) = PIJ(I,48) 
C                
        BXY5(I) = PIJ(I,49) 
        BYX5(I) = PIJ(I,50) 
        BXZ5(I) = PIJ(I,51) 
        BZX5(I) = PIJ(I,52) 
        BYZ5(I) = PIJ(I,53) 
        BZY5(I) = PIJ(I,54) 
C                
        BXY6(I) = PIJ(I,55) 
        BYX6(I) = PIJ(I,56) 
        BXZ6(I) = PIJ(I,57) 
        BZX6(I) = PIJ(I,58) 
        BYZ6(I) = PIJ(I,59) 
        BZY6(I) = PIJ(I,60) 
C                
        BXY7(I) = PIJ(I,61) 
        BYX7(I) = PIJ(I,62) 
        BXZ7(I) = PIJ(I,63) 
        BZX7(I) = PIJ(I,64) 
        BYZ7(I) = PIJ(I,65) 
        BZY7(I) = PIJ(I,66) 
C                
        BXY8(I) = PIJ(I,67) 
        BYX8(I) = PIJ(I,68) 
        BXZ8(I) = PIJ(I,69) 
        BZX8(I) = PIJ(I,70) 
        BYZ8(I) = PIJ(I,71) 
        BZY8(I) = PIJ(I,72) 
       END DO
C-----------
      RETURN
      END
