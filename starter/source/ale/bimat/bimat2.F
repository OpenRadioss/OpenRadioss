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
Chd|  BIMAT2                        source/ale/bimat/bimat2.F     
Chd|-- called by -----------
Chd|        BINIT2                        source/ale/bimat/binit2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BIMAT2(PM,MXT,VOLT,ALPH,FILL,
     .                  VOL,OFF,IX)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(NIXQ,MVSIZ), MXT(MVSIZ)
      my_real
     .   PM(NPROPM,*),VOLT(MVSIZ),ALPH(MVSIZ),FILL(*),VOL(MVSIZ),
     .   OFF(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MT
      INTEGER IX1, IX2, IX3, IX4
      my_real
     .   ALPHN, ALPHD

C-----------------------------------------------
C=======================================================================
      DO I=LFT,LLT
        OFF(I)=ONE
        ALPH(I) = ZERO
        MT=IABS(IX(1,I))
        IX1 = IX(2, I)
        IX2 = IX(3, I)
        IX3 = IX(4, I)
        IX4 = IX(5, I)
        MXT(I)=NINT(PM(20+JMULT,MT))
        ALPHN= MAX(FILL(IX1),ZERO)
     .       + MAX(FILL(IX2),ZERO)
     .       + MAX(FILL(IX3),ZERO)
     .       + MAX(FILL(IX4),ZERO)
        ALPHD=ABS(FILL(IX1))
     .       +ABS(FILL(IX2))
     .       +ABS(FILL(IX3))
     .       +ABS(FILL(IX4))
        IF(ALPHD>EM20)ALPH(I)=ALPHN/ALPHD
        VOL(I)=ALPH(I)*VOLT(I)
      ENDDO
C-----------
      RETURN
      END
