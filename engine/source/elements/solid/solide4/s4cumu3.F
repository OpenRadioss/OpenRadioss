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
Chd|  S4CUMU3                       source/elements/solid/solide4/s4cumu3.F
Chd|-- called by -----------
Chd|        MULTI_FVM2FEM                 source/multifluid/multi_fvm2fem.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4CUMU3(
     1   OFFG,    E,       NC1,     NC2,
     2   NC3,     NC4,     STIFN,   STI,
     3   F11,     F21,     F31,     F12,
     4   F22,     F32,     F13,     F23,
     5   F33,     F14,     F24,     F34,
     6   THEM,    FTHE,    CONDN,   CONDE,
     7   NEL,     JTHE)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JTHE
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C     REAL
      my_real
     .   OFFG(*),E(3,*),STIFN(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   THEM(MVSIZ,4),FTHE(*),CONDN(*),CONDE(*)
CMasParINCLUDE 'scumu3.intmap.inc'
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,J
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
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
           STI(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
      IF(JTHE < 0 ) THEN
       IF(OFF_L<=ZERO)THEN
        DO J=1,4
         DO I=1,NEL
          IF(OFFG(I)<=ZERO)THEN
             THEM(I,J)=ZERO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF(NODADT_THERM == 1) THEN
        IF(OFF_L<ZERO)THEN
         DO I=1,NEL
          IF(OFFG(I)<ZERO)THEN
             CONDE(I)=ZERO
          ENDIF
         ENDDO
        ENDIF
       ENDIF  
      ENDIF
C-----------------------------------------------
C
C     because 2*Mnodal = 2*Melement/4
      DO I=1,NEL
        STI(I)=HALF*STI(I)
      END DO
      IF(NODADT_THERM == 1 ) THEN  
        DO I=1,NEL
           CONDE(I)=FOURTH*CONDE(I)
        END DO
      ENDIF
C
       IF(JTHE < 0 )  THEN
         IF(NODADT_THERM == 1) THEN
          DO  I=1,NEL
C
          E(1,NC1(I))=E(1,NC1(I))+F11(I)
          E(2,NC1(I))=E(2,NC1(I))+F21(I)
          E(3,NC1(I))=E(3,NC1(I))+F31(I)
          STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
          FTHE(NC1(I)) = FTHE(NC1(I)) + THEM(I,1)
          CONDN(NC1(I))= CONDN(NC1(I))+CONDE(I)
C
          E(1,NC2(I))=E(1,NC2(I))+F12(I)
          E(2,NC2(I))=E(2,NC2(I))+F22(I)
          E(3,NC2(I))=E(3,NC2(I))+F32(I)
          STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
          FTHE(NC2(I)) = FTHE(NC2(I)) + THEM(I,2)
          CONDN(NC2(I))= CONDN(NC2(I))+ CONDE(I)
C
          E(1,NC3(I))=E(1,NC3(I))+F13(I)
          E(2,NC3(I))=E(2,NC3(I))+F23(I)
          E(3,NC3(I))=E(3,NC3(I))+F33(I)
          STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
          FTHE(NC3(I)) = FTHE(NC3(I)) + THEM(I,3)
          CONDN(NC3(I))= CONDN(NC3(I))+ CONDE(I)
C
          E(1,NC4(I))=E(1,NC4(I))+F14(I)
          E(2,NC4(I))=E(2,NC4(I))+F24(I)
          E(3,NC4(I))=E(3,NC4(I))+F34(I)
          STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
          FTHE(NC4(I)) = FTHE(NC4(I)) + THEM(I,4)
          CONDN(NC4(I))= CONDN(NC4(I))+ CONDE(I)
C
         ENDDO
        ELSE
          DO  I=1,NEL
C
          E(1,NC1(I))=E(1,NC1(I))+F11(I)
          E(2,NC1(I))=E(2,NC1(I))+F21(I)
          E(3,NC1(I))=E(3,NC1(I))+F31(I)
          STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
          FTHE(NC1(I)) = FTHE(NC1(I)) + THEM(I,1)
C
          E(1,NC2(I))=E(1,NC2(I))+F12(I)
          E(2,NC2(I))=E(2,NC2(I))+F22(I)
          E(3,NC2(I))=E(3,NC2(I))+F32(I)
          STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
          FTHE(NC2(I)) = FTHE(NC2(I)) + THEM(I,2)
C
          E(1,NC3(I))=E(1,NC3(I))+F13(I)
          E(2,NC3(I))=E(2,NC3(I))+F23(I)
          E(3,NC3(I))=E(3,NC3(I))+F33(I)
          STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
          FTHE(NC3(I)) = FTHE(NC3(I)) + THEM(I,3)
C
          E(1,NC4(I))=E(1,NC4(I))+F14(I)
          E(2,NC4(I))=E(2,NC4(I))+F24(I)
          E(3,NC4(I))=E(3,NC4(I))+F34(I)
          STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
          FTHE(NC4(I)) = FTHE(NC4(I)) + THEM(I,4)
C
         ENDDO
        ENDIF
       ELSE  
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
          E(1,NC4(I))=E(1,NC4(I))+F14(I)
          E(2,NC4(I))=E(2,NC4(I))+F24(I)
          E(3,NC4(I))=E(3,NC4(I))+F34(I)
          STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
C
  100     CONTINUE       
       
       ENDIF
 
      RETURN
      END
