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
Chd|  I7LIN3                        source/interfaces/inter3d1/i7lin3.F
Chd|-- called by -----------
Chd|        I2BAR3                        source/interfaces/inter3d1/i2dst3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I7LIN3(XI,YI,ZI,XA,YA,ZA,XB,YB,ZB,NX,NY,NZ,P,ALP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .    XI,YI,ZI,XA,YA,ZA,XB,YB,ZB,NX,NY,NZ,P,ALP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .    XAB,YAB,ZAB,
     .    XP,YP,ZP,NORM
C
      XAB = XB - XA
      YAB = YB - YA
      ZAB = ZB - ZA
C
      ALP = (XI-XA)*XAB + (YI-YA)*YAB + (ZI-ZA)*ZAB
      NORM = MAX(EM20, XAB**2 + YAB**2 + ZAB**2)
      ALP = ALP / NORM
      ALP = MAX(ZERO,ALP)
      ALP = MIN(ONE,ALP)
C
      XP = XA + ALP*XAB 
      YP = YA + ALP*YAB 
      ZP = ZA + ALP*ZAB
C
      NX = XI -XP 
      NY = YI -YP 
      NZ = ZI -ZP
C
      P = SQRT(NX**2 + NY**2 +NZ**2)
      NORM = MAX(EM20,P)
C
      NX = NX / NORM
      NY = NY / NORM
      NZ = NZ / NORM
C
      RETURN
      END
