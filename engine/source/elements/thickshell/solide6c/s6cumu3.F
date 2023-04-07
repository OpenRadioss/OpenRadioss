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
Chd|  S6CUMU3                       source/elements/thickshell/solide6c/s6cumu3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CUMU3(
     1   OFFG,    E,       NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   STIFN,   STI,     F11,     F21,
     4   F31,     F12,     F22,     F32,
     5   F13,     F23,     F33,     F14,
     6   F24,     F34,     F15,     F25,
     7   F35,     F16,     F26,     F36,
     8   NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*)
C     REAL
      my_real
     .   OFFG(*),E(3,*),STIFN(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C-----------------------------------------------
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=1,NEL
Cf small3b        IF(OFF(I)<1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO I=1,NEL
         IF(OFFG(I)<ZERO)THEN
           F11(I)=ZERO
           F21(I)=ZERO
           F31(I)=ZERO
           F12(I)=ZERO
           F22(I)=ZERO
           F32(I)=ZERO
           F13(I)=ZERO
           F23(I)=ZERO
           F33(I)=ZERO
           F14(I)=ZERO
           F24(I)=ZERO
           F34(I)=ZERO
           F15(I)=ZERO
           F25(I)=ZERO
           F35(I)=ZERO
           F16(I)=ZERO
           F26(I)=ZERO
           F36(I)=ZERO
           STI(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C
C     because 2*Mnodal = 2*Melement/6
      DO I=1,NEL
        STI(I)=THIRD*STI(I)
      END DO
C
      DO 100 I=1,NEL
C
      E(1,NC1(I))=E(1,NC1(I))+F11(I)
      E(2,NC1(I))=E(2,NC1(I))+F21(I)
      E(3,NC1(I))=E(3,NC1(I))+F31(I)
      STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
C
      E(1,NC2(I))=E(1,NC2(I))+F12(I)
      E(2,NC2(I))=E(2,NC2(I))+F22(I)
      E(3,NC2(I))=E(3,NC2(I))+F32(I)
      STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
C
      E(1,NC3(I))=E(1,NC3(I))+F13(I)
      E(2,NC3(I))=E(2,NC3(I))+F23(I)
      E(3,NC3(I))=E(3,NC3(I))+F33(I)
      STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
C
      E(1,NC5(I))=E(1,NC5(I))+F15(I)
      E(2,NC5(I))=E(2,NC5(I))+F25(I)
      E(3,NC5(I))=E(3,NC5(I))+F35(I)
      STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
C
      E(1,NC4(I))=E(1,NC4(I))+F14(I)
      E(2,NC4(I))=E(2,NC4(I))+F24(I)
      E(3,NC4(I))=E(3,NC4(I))+F34(I)
      STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
C
      E(1,NC6(I))=E(1,NC6(I))+F16(I)
      E(2,NC6(I))=E(2,NC6(I))+F26(I)
      E(3,NC6(I))=E(3,NC6(I))+F36(I)
      STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
C
  100 CONTINUE
      RETURN
      END
