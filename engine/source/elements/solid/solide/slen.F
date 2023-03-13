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
      SUBROUTINE SLEN(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,J,AREA,AREAM)
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
      INTEGER J
      my_real
     .   X1(*), X2(*), X3(*), X4(*), Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*),AREA(MVSIZ,6),AREAM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
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
      DO 10 I=LFT,LLT
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
Chd|====================================================================
Chd|  SLENA                         source/elements/solid/solide/slen.F
Chd|-- called by -----------
Chd|        FDLEN3                        source/elements/solid/solide/fdlen3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SLENA(
     .   N1X, N2X, N3X, N4X, N5X, N6X,
     .   N1Y, N2Y, N3Y, N4Y, N5Y, N6Y,
     .   N1Z, N2Z, N3Z, N4Z, N5Z, N6Z,
     .   X1, X2, X3, X4, X5, X6, X7, X8,
     .   Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .   Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,AREAM)
C
C NOTE : SURFACE OF FACETS IS AVAILABLE FOR ALE ELEMENTS
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER J
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   N1X(*), N2X(*), N3X(*), N4X(*), N5X(*), N6X(*),
     .   N1Y(*), N2Y(*), N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .   N1Z(*), N2Z(*), N3Z(*), N4Z(*), N5Z(*), N6Z(*),
     .   AREAM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   AREA(6)
C-----------------------------------------------
      DO I=LFT,LLT
        N1X(I)=(Y3(I)-Y1(I))*(Z2(I)-Z4(I)) - (Z3(I)-Z1(I))*(Y2(I)-Y4(I))
        N1Y(I)=(Z3(I)-Z1(I))*(X2(I)-X4(I)) - (X3(I)-X1(I))*(Z2(I)-Z4(I))
        N1Z(I)=(X3(I)-X1(I))*(Y2(I)-Y4(I)) - (Y3(I)-Y1(I))*(X2(I)-X4(I))
        AREA(1)=N1X(I)**2+N1Y(I)**2+N1Z(I)**2
C
        N2X(I)=(Y7(I)-Y4(I))*(Z3(I)-Z8(I)) - (Z7(I)-Z4(I))*(Y3(I)-Y8(I))
        N2Y(I)=(Z7(I)-Z4(I))*(X3(I)-X8(I)) - (X7(I)-X4(I))*(Z3(I)-Z8(I))
        N2Z(I)=(X7(I)-X4(I))*(Y3(I)-Y8(I)) - (Y7(I)-Y4(I))*(X3(I)-X8(I))
        AREA(2)=N2X(I)**2+N2Y(I)**2+N2Z(I)**2
C
        N3X(I)=(Y6(I)-Y8(I))*(Z7(I)-Z5(I)) - (Z6(I)-Z8(I))*(Y7(I)-Y5(I))
        N3Y(I)=(Z6(I)-Z8(I))*(X7(I)-X5(I)) - (X6(I)-X8(I))*(Z7(I)-Z5(I))
        N3Z(I)=(X6(I)-X8(I))*(Y7(I)-Y5(I)) - (Y6(I)-Y8(I))*(X7(I)-X5(I))
        AREA(3)=N3X(I)**2+N3Y(I)**2+N3Z(I)**2
C
        N4X(I)=(Y2(I)-Y5(I))*(Z6(I)-Z1(I)) - (Z2(I)-Z5(I))*(Y6(I)-Y1(I))
        N4Y(I)=(Z2(I)-Z5(I))*(X6(I)-X1(I)) - (X2(I)-X5(I))*(Z6(I)-Z1(I))
        N4Z(I)=(X2(I)-X5(I))*(Y6(I)-Y1(I)) - (Y2(I)-Y5(I))*(X6(I)-X1(I))
        AREA(4)=N4X(I)**2+N4Y(I)**2+N4Z(I)**2
C
        N5X(I)=(Y7(I)-Y2(I))*(Z6(I)-Z3(I)) - (Z7(I)-Z2(I))*(Y6(I)-Y3(I))
        N5Y(I)=(Z7(I)-Z2(I))*(X6(I)-X3(I)) - (X7(I)-X2(I))*(Z6(I)-Z3(I))
        N5Z(I)=(X7(I)-X2(I))*(Y6(I)-Y3(I)) - (Y7(I)-Y2(I))*(X6(I)-X3(I))
        AREA(5)=N5X(I)**2+N5Y(I)**2+N5Z(I)**2
C
        N6X(I)=(Y8(I)-Y1(I))*(Z4(I)-Z5(I)) - (Z8(I)-Z1(I))*(Y4(I)-Y5(I))
        N6Y(I)=(Z8(I)-Z1(I))*(X4(I)-X5(I)) - (X8(I)-X1(I))*(Z4(I)-Z5(I))
        N6Z(I)=(X8(I)-X1(I))*(Y4(I)-Y5(I)) - (Y8(I)-Y1(I))*(X4(I)-X5(I))
        AREA(6)=N6X(I)**2+N6Y(I)**2+N6Z(I)**2
        AREAM(I)= MAX(AREA(1),AREA(2),AREA(3),AREA(4),AREA(5),AREA(6))
      ENDDO
      RETURN
      END
