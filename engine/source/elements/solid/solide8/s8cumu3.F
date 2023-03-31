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
Chd|  S8CUMU3                       source/elements/solid/solide8/s8cumu3.F
Chd|-- called by -----------
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8CUMU3(
     1   OFFG,    E,       NC,      STIFN,
     2   STI,     F11,     F21,     F31,
     3   F12,     F22,     F32,     F13,
     4   F23,     F33,     F14,     F24,
     5   F34,     F15,     F25,     F35,
     6   F16,     F26,     F36,     F17,
     7   F27,     F37,     F18,     F28,
     8   F38,     NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NC(8,*)
      my_real
     .   OFFG(*),E(3,*),STIFN(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*)
CMasParINCLUDE 'scumu3.intmap.inc'
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C-----------------------------------------------
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=1,NEL
Cf small3b        IF(OFF(I)<1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<0.)THEN
        DO I=1,NEL
          IF(OFFG(I)<0.)THEN
            F11(I)=0.
            F21(I)=0.
            F31(I)=0.
            F12(I)=0.
            F22(I)=0.
            F32(I)=0.
            F13(I)=0.
            F23(I)=0.
            F33(I)=0.
            F14(I)=0.
            F24(I)=0.
            F34(I)=0.
            F15(I)=0.
            F25(I)=0.
            F35(I)=0.
            F16(I)=0.
            F26(I)=0.
            F36(I)=0.
            F17(I)=0.
            F27(I)=0.
            F37(I)=0.
            F18(I)=0.
            F28(I)=0.
            F38(I)=0.
            STI(I)=0.
          ENDIF
        ENDDO
      ENDIF
C
C     because 2*Mnodal = 2*Melement/8
      DO I=1,NEL
        STI(I)=FOURTH*STI(I)
      END DO
C
      DO 100 I=1,NEL
C
      E(1,NC(1,I))=E(1,NC(1,I))+F11(I)
      E(2,NC(1,I))=E(2,NC(1,I))+F21(I)
      E(3,NC(1,I))=E(3,NC(1,I))+F31(I)
      STIFN(NC(1,I))=STIFN(NC(1,I))+STI(I)
C
      E(1,NC(7,I))=E(1,NC(7,I))+F17(I)
      E(2,NC(7,I))=E(2,NC(7,I))+F27(I)
      E(3,NC(7,I))=E(3,NC(7,I))+F37(I)
      STIFN(NC(7,I))=STIFN(NC(7,I))+STI(I)
C
      E(1,NC(2,I))=E(1,NC(2,I))+F12(I)
      E(2,NC(2,I))=E(2,NC(2,I))+F22(I)
      E(3,NC(2,I))=E(3,NC(2,I))+F32(I)
      STIFN(NC(2,I))=STIFN(NC(2,I))+STI(I)
C
      E(1,NC(8,I))=E(1,NC(8,I))+F18(I)
      E(2,NC(8,I))=E(2,NC(8,I))+F28(I)
      E(3,NC(8,I))=E(3,NC(8,I))+F38(I)
      STIFN(NC(8,I))=STIFN(NC(8,I))+STI(I)
C
      E(1,NC(3,I))=E(1,NC(3,I))+F13(I)
      E(2,NC(3,I))=E(2,NC(3,I))+F23(I)
      E(3,NC(3,I))=E(3,NC(3,I))+F33(I)
      STIFN(NC(3,I))=STIFN(NC(3,I))+STI(I)
C
      E(1,NC(5,I))=E(1,NC(5,I))+F15(I)
      E(2,NC(5,I))=E(2,NC(5,I))+F25(I)
      E(3,NC(5,I))=E(3,NC(5,I))+F35(I)
      STIFN(NC(5,I))=STIFN(NC(5,I))+STI(I)
C
      E(1,NC(4,I))=E(1,NC(4,I))+F14(I)
      E(2,NC(4,I))=E(2,NC(4,I))+F24(I)
      E(3,NC(4,I))=E(3,NC(4,I))+F34(I)
      STIFN(NC(4,I))=STIFN(NC(4,I))+STI(I)
C
      E(1,NC(6,I))=E(1,NC(6,I))+F16(I)
      E(2,NC(6,I))=E(2,NC(6,I))+F26(I)
      E(3,NC(6,I))=E(3,NC(6,I))+F36(I)
      STIFN(NC(6,I))=STIFN(NC(6,I))+STI(I)
C
  100 CONTINUE
      RETURN
      END
