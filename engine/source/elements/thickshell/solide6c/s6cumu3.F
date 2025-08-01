Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    s6cumu3    ../engine/source/elements/thickshell/solide6c/s6cumu3.F
!||--- called by ------------------------------------------------------
!||    s6cforc3   ../engine/source/elements/thickshell/solide6c/s6cforc3.F
!||====================================================================
      SUBROUTINE S6CUMU3(
     1   OFFG,    E,       NC1,     NC2,
     2   NC3,     NC4,     NC5,     NC6,
     3   STIFN,   STI,     F11,     F21,
     4   F31,     F12,     F22,     F32,
     5   F13,     F23,     F33,     F14,
     6   F24,     F34,     F15,     F25,
     7   F35,     F16,     F26,     F36,
     8   NEL,    JTHE,    FTHE,    THEM,
     9   CONDN, CONDE,   IFTHE,  ICONDN,
     .   NODADT_THERM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL,JTHE,IFTHE,ICONDN
      INTEGER, INTENT(IN) :: NODADT_THERM
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*)
C     REAL
      my_real
     .   OFFG(*),E(3,*),STIFN(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*)
      my_real, INTENT(INOUT) :: THEM(MVSIZ,6),FTHE(IFTHE),
     .   CONDN(ICONDN),CONDE(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C-----------------------------------------------
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = ZERO
      DO I=1,NEL
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
      IF (JTHE < 0) THEN
        IF (OFF_L<=ZERO) THEN
          DO J=1,6
            DO I=1,NEL
              IF(OFFG(I)<=ZERO)THEN
                THEM(I,J)=ZERO
              ENDIF
            ENDDO
          ENDDO
        ENDIF
        IF (NODADT_THERM == 1) THEN
          IF(OFF_L<ZERO)THEN
            DO I=1,NEL
              IF(OFFG(I)<ZERO)THEN
                CONDE(I)=ZERO
              ENDIF
            ENDDO
          ENDIF
        ENDIF  
      ENDIF
C
C     because 2*Mnodal = 2*Melement/6
      DO I=1,NEL
        STI(I)=THIRD*STI(I)
      END DO
      IF (NODADT_THERM == 1) THEN  
        DO I = 1,NEL
          CONDE(I) = ONE_OVER_6*CONDE(I)
        END DO
      ENDIF
C
      DO I=1,NEL
C
        IF(JTHE >= 0) THEN
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
CCC
        ELSE
C
          E(1,NC1(I))=E(1,NC1(I))+F11(I)
          E(2,NC1(I))=E(2,NC1(I))+F21(I)
          E(3,NC1(I))=E(3,NC1(I))+F31(I)
          STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
          FTHE(NC1(I)) = FTHE(NC1(I)) + THEM(I,1)
          IF(NODADT_THERM == 1) CONDN(NC1(I))= CONDN(NC1(I))+ CONDE(I)
C
          E(1,NC2(I))=E(1,NC2(I))+F12(I)
          E(2,NC2(I))=E(2,NC2(I))+F22(I)
          E(3,NC2(I))=E(3,NC2(I))+F32(I)
          STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
          FTHE(NC2(I)) = FTHE(NC2(I)) + THEM(I,2)
          IF(NODADT_THERM == 1) CONDN(NC2(I))= CONDN(NC2(I))+ CONDE(I)
C
          E(1,NC3(I))=E(1,NC3(I))+F13(I)
          E(2,NC3(I))=E(2,NC3(I))+F23(I)
          E(3,NC3(I))=E(3,NC3(I))+F33(I)
          STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
          FTHE(NC3(I)) = FTHE(NC3(I)) + THEM(I,3)
          IF(NODADT_THERM == 1) CONDN(NC3(I))= CONDN(NC3(I))+ CONDE(I)
C
          E(1,NC5(I))=E(1,NC5(I))+F15(I)
          E(2,NC5(I))=E(2,NC5(I))+F25(I)
          E(3,NC5(I))=E(3,NC5(I))+F35(I)
          STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
          FTHE(NC5(I)) = FTHE(NC5(I)) + THEM(I,5)
          IF(NODADT_THERM == 1) CONDN(NC5(I))= CONDN(NC5(I))+ CONDE(I)
C
          E(1,NC4(I))=E(1,NC4(I))+F14(I)
          E(2,NC4(I))=E(2,NC4(I))+F24(I)
          E(3,NC4(I))=E(3,NC4(I))+F34(I)
          STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
          FTHE(NC4(I)) = FTHE(NC4(I)) + THEM(I,4)
          IF(NODADT_THERM == 1) CONDN(NC4(I))= CONDN(NC4(I))+ CONDE(I)
C
          E(1,NC6(I))=E(1,NC6(I))+F16(I)
          E(2,NC6(I))=E(2,NC6(I))+F26(I)
          E(3,NC6(I))=E(3,NC6(I))+F36(I)
          STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
          FTHE(NC6(I)) = FTHE(NC6(I)) + THEM(I,6)
          IF(NODADT_THERM == 1)CONDN(NC6(I))= CONDN(NC6(I))+ CONDE(I)
C
        ENDIF
C
      ENDDO
      RETURN
      END
