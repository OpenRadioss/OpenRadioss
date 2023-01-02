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
Chd|        S6CDERI3                      source/elements/thickshell/solide6c/s6cderi3.F
Chd|        SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SLEN(X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,J,
     .     AREA, AREAM)
C-----------------------------------------------------------
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
     .   Z1(*), Z2(*), Z3(*), Z4(*),
     .   AREA(6, *), AREAM(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   X13(MVSIZ), X24(MVSIZ), Y13(MVSIZ),
     .   Y24(MVSIZ), Z13(MVSIZ), Z24(MVSIZ), 
     .   FS1(MVSIZ), FS2(MVSIZ), FS3(MVSIZ), FT1(MVSIZ), FT2(MVSIZ),
     .   FT3(MVSIZ), E(MVSIZ), F(MVSIZ), G(MVSIZ)
C-----------------------------------------------------------------------
C
      DO I=LFT,LLT
        X13(I)    = X3(I)-X1(I)
        X24(I)    = X4(I)-X2(I)
        Y13(I)    = Y3(I)-Y1(I)
        Y24(I)    = Y4(I)-Y2(I)
        Z13(I)    = Z3(I)-Z1(I)
        Z24(I)    = Z4(I)-Z2(I)
        FS1(I)    = X13(I)-X24(I)
        FT1(I)    = X13(I)+X24(I)
        FS2(I)    = Y13(I)-Y24(I)
        FT2(I)    = Y13(I)+Y24(I)
        FS3(I)    = Z13(I)-Z24(I)
        FT3(I)    = Z13(I)+Z24(I)
        E(I)      = FS1(I)*FS1(I)+FS2(I)*FS2(I)+FS3(I)*FS3(I)
        F(I)      = FS1(I)*FT1(I)+FS2(I)*FT2(I)+FS3(I)*FT3(I)
        G(I)      = FT1(I)*FT1(I)+FT2(I)*FT2(I)+FT3(I)*FT3(I)
        AREA(J,I) = E(I)*G(I)-F(I)*F(I)
        AREAM(I)  = MAX(AREA(J,I),AREAM(I))
      END DO
      
      RETURN
      END
