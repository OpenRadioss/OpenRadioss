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
C                                                                     12
Chd|====================================================================
Chd|  SZORDEF3                      source/elements/solid/solidez/szordef3.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SROTO3V                       source/elements/solid/solidez/sroto3v.F
Chd|====================================================================
      SUBROUTINE SZORDEF3(JFT,JLT,DXX,DYY,DZZ,D4,D5,D6,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
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
      INTEGER JFT, JLT
C     REAL
      my_real
     .   DXX(*),DYY(*),DZZ(*),D4(*),D5(*),D6(*), G1X(*),G1Y(*),G1Z(*),
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DEF(MVSIZ,6),DEFN(MVSIZ,6)
C=======================================================================
      DO I=JFT,JLT
        DEF(I,1) = DXX(I)
        DEF(I,2) = DYY(I)
        DEF(I,3) = DZZ(I)
        DEF(I,4) = HALF*D4(I)
        DEF(I,5) = HALF*D5(I)
        DEF(I,6) = HALF*D6(I)
      ENDDO
      CALL SROTO3V(JFT,JLT,DEF,DEFN,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z)
      DO I=JFT,JLT
        DXX(I) = DEFN(I,1)
        DYY(I) = DEFN(I,2)
        DZZ(I) = DEFN(I,3)
        D4(I) = TWO*DEFN(I,4)
        D5(I) = TWO*DEFN(I,5)
        D6(I) = TWO*DEFN(I,6)
      ENDDO
C-----------
      RETURN
      END
