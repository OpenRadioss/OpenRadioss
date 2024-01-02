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
Chd|  SORDEF12                      source/elements/solid/solidez/sordef12.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|        SROTO12                       source/elements/solid/solidez/sroto12.F
Chd|====================================================================
      SUBROUTINE SORDEF12(JFT,JLT,DXX,DYY,DZZ,D4,D5,D6,
     .                   G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
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
     .   G2X(*),G2Y(*),G2Z(*),G3X(*),G3Y(*),G3Z(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DEF(MVSIZ,6)
C=======================================================================
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
        DEF(I,1) = DXX(I)
        DEF(I,2) = DYY(I)
        DEF(I,3) = DZZ(I)
        DEF(I,4) = HALF*D4(I)
        DEF(I,5) = HALF*D5(I)
        DEF(I,6) = HALF*D6(I)
      ENDDO
      CALL SROTO12(JFT,JLT,DEF,
     .                 G1X,G1Y,G1Z,G2X,G2Y,G2Z,G3X,G3Y,G3Z,OFF)
      DO I=JFT,JLT
       IF (OFF(I) <=ONE) CYCLE
        DXX(I) = DEF(I,1)
        DYY(I) = DEF(I,2)
        DZZ(I) = DEF(I,3)
        D4(I) = TWO*DEF(I,4)
        D5(I) = TWO*DEF(I,5)
        D6(I) = TWO*DEF(I,6)
      ENDDO
C-----------
      RETURN
      END
