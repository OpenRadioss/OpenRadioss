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
Chd|  SCORDEF3                      source/elements/thickshell/solidec/scordef3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|        SCROTO                        source/elements/thickshell/solidec/scroto.F
Chd|====================================================================
      SUBROUTINE SCORDEF3(NEL,DXX,DYY,DZZ,D4,D5,D6,DIR)
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
      INTEGER :: NEL
C
      my_real
     .   DXX(*),DYY(*),DZZ(*),D4(*),D5(*),D6(*), DIR(MVSIZ,2)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   DEF(MVSIZ,6),DEFN(MVSIZ,6),DIRN(MVSIZ,2)
C-----------------------------------------------
      DO I=1,NEL
        DEF(I,1) = DXX(I)
        DEF(I,2) = DYY(I)
        DEF(I,3) = DZZ(I)
        DEF(I,4) = HALF*D4(I)
        DEF(I,5) = HALF*D5(I)
        DEF(I,6) = HALF*D6(I)
        DIRN(I,1)=DIR(I,1)
        DIRN(I,2)=-DIR(I,2)
      ENDDO
      CALL SCROTO(NEL,DEF,DEFN,DIRN)
      DO I=1,NEL
        DXX(I) = DEFN(I,1)
        DYY(I) = DEFN(I,2)
        D4(I) = TWO*DEFN(I,4)
        D5(I) = TWO*DEFN(I,5)
        D6(I) = TWO*DEFN(I,6)
      ENDDO
C-----------
      RETURN
      END
