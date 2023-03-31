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
Chd|  SFILLOPT                      source/elements/solid/solide/sfillopt.F
Chd|-- called by -----------
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SFILLOPT(
     1   FILL,    STI,     F11,     F21,
     2   F31,     F12,     F22,     F32,
     3   F13,     F23,     F33,     F14,
     4   F24,     F34,     F15,     F25,
     5   F35,     F16,     F26,     F36,
     6   F17,     F27,     F37,     F18,
     7   F28,     F38,     NEL)
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
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*)
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
        F15(I)=FILL(I)*F15(I)
        F25(I)=FILL(I)*F25(I)
        F35(I)=FILL(I)*F35(I)
        F16(I)=FILL(I)*F16(I)
        F26(I)=FILL(I)*F26(I)
        F36(I)=FILL(I)*F36(I)
        F17(I)=FILL(I)*F17(I)
        F27(I)=FILL(I)*F27(I)
        F37(I)=FILL(I)*F37(I)
        F18(I)=FILL(I)*F18(I)
        F28(I)=FILL(I)*F28(I)
        F38(I)=FILL(I)*F38(I)
      END DO              

      RETURN
      END
