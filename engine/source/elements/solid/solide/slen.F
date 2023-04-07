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
Chd|  SLEN                          source/elements/solid/solide/slen.F
Chd|-- called by -----------
Chd|        FDLEN3                        source/elements/solid/solide/fdlen3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SLEN(
     1   X1,      X2,      X3,      X4,
     2   Y1,      Y2,      Y3,      Y4,
     3   Z1,      Z2,      Z3,      Z4,
     4   J,       AREA,    AREAM,   NEL)
C
C NOTE : SURFACE OF FACETS IS AVAILABLE FOR ALE ELEMENTS
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER J
      my_real
     .   X1(*), X2(*), X3(*), X4(*), Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*),AREA(MVSIZ,6),AREAM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   X13, Y13, Z13,
     .   X24, Y24, Z24,
     .   FS1, FS2, FS3,
     .   FT1, FT2, FT3,
     .   E, F, G
C-----------------------------------------------
      DO 10 I=1,NEL
      X13=X3(I) - X1(I)
      X24=X4(I) - X2(I)
      Y13=Y3(I) - Y1(I)
      Y24=Y4(I) - Y2(I)
      Z13=Z3(I) - Z1(I)
      Z24=Z4(I) - Z2(I)
C
      FS1=X13-X24
      FT1=X13+X24
      FS2=Y13-Y24
      FT2=Y13+Y24
      FS3=Z13-Z24
      FT3=Z13+Z24
C
      E=FS1*FS1+FS2*FS2+FS3*FS3
      F=FS1*FT1+FS2*FT2+FS3*FT3
      G=FT1*FT1+FT2*FT2+FT3*FT3
      AREA(I,J) =E*G-F*F
      AREAM(I)= MAX(AREA(I,J),AREAM(I))
   10 CONTINUE
      RETURN
      END
