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
Chd|  S8EA2BP8                      source/elements/solid/solide8e/s8ea2bp8.F
Chd|-- called by -----------
Chd|        S8EDERISH2                    source/elements/solid/solide8e/s8ederish2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EA2BP8(
     1   PX1,     PX2,     PX3,     PX4,
     2   PX5,     PX6,     PX7,     PX8,
     3   P0X1,    P0X2,    P0X3,    P0X4,
     4   P0X5,    P0X6,    P0X7,    P0X8,
     5   NEL)
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
     .   P0X1(*), P0X2(*), P0X3(*), P0X4(*),
     .   P0X5(*), P0X6(*), P0X7(*), P0X8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C      
        PX1(1:NEL) = P0X1(1:NEL)  
        PX2(1:NEL) = P0X2(1:NEL)  
        PX3(1:NEL) = P0X3(1:NEL)  
        PX4(1:NEL) = P0X4(1:NEL)  
        PX5(1:NEL) = P0X5(1:NEL)  
        PX6(1:NEL) = P0X6(1:NEL)  
        PX7(1:NEL) = P0X7(1:NEL)  
        PX8(1:NEL) = P0X8(1:NEL)  
C
      RETURN
       END
