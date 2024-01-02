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
Chd|  S4FILLOPT                     source/elements/solid/solide4/s4fillopt.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4FILLOPT(
     1   FILL,    STI,     F11,     F21,
     2   F31,     F12,     F22,     F32,
     3   F13,     F23,     F33,     F14,
     4   F24,     F34,     NEL)
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
      my_real
     .   FILL(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO I=1,NEL
        STI(I)=FILL(I)*STI(I)
        F11(I)=FILL(I)*F11(I)
        F21(I)=FILL(I)*F21(I)
        F31(I)=FILL(I)*F31(I)
        F12(I)=FILL(I)*F12(I)
        F22(I)=FILL(I)*F22(I)
        F32(I)=FILL(I)*F32(I)
        F13(I)=FILL(I)*F13(I)
        F23(I)=FILL(I)*F23(I)
        F33(I)=FILL(I)*F33(I)
        F14(I)=FILL(I)*F14(I)
        F24(I)=FILL(I)*F24(I)
        F34(I)=FILL(I)*F34(I)
      END DO              

      RETURN
      END
