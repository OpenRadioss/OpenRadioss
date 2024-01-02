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
Chd|  SDLEN3                        source/elements/solid/solide/sdlen3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|        SLDEGE                        source/elements/solid/solide/sldege.F
Chd|        SLEN                          source/elements/solid/solide/slen.F
Chd|        SLENA                         source/elements/solid/solide/slena.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|====================================================================
      SUBROUTINE SDLEN3(
     1   VOLN,    DELTAX,  X1,      X2,
     2   X3,      X4,      X5,      X6,
     3   X7,      X8,      Y1,      Y2,
     4   Y3,      Y4,      Y5,      Y6,
     5   Y7,      Y8,      Z1,      Z2,
     6   Z3,      Z4,      Z5,      Z6,
     7   Z7,      Z8,      N1X,     N2X,
     8   N3X,     N4X,     N5X,     N6X,
     9   N1Y,     N2Y,     N3Y,     N4Y,
     A   N5Y,     N6Y,     N1Z,     N2Z,
     B   N3Z,     N4Z,     N5Z,     N6Z,
     C   NEL,     MTN,     JALE,    JEUL)
C-----------------------------------------------
C   M o d u l e  s
C-----------------------------------------------
      USE ALE_MOD
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
      INTEGER, INTENT(IN) :: MTN
      INTEGER, INTENT(IN) :: JALE
      INTEGER, INTENT(IN) :: JEUL
      my_real
     .   VOLN(*),DELTAX(*),
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),
     .   N1X(*), N2X(*), N3X(*), N4X(*), N5X(*), N6X(*),
     .   N1Y(*), N2Y(*), N3Y(*), N4Y(*), N5Y(*), N6Y(*),
     .   N1Z(*), N2Z(*), N3Z(*), N4Z(*), N5Z(*), N6Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NFAC
      my_real XIOFF(MVSIZ), AREAM(MVSIZ), ATEST(MVSIZ), AREA(MVSIZ,6)
C-----------------------------------------------
      DO I=1,NEL
        XIOFF(I) = ONE
        AREAM(I) = EM20
      ENDDO

      IF((ALE%GLOBAL%ICAA==1 .OR. ALE%GLOBAL%ISFINT>=2) .AND. JALE+JEUL/=0)THEN
        CALL SLENA(
     1   N1X,     N2X,     N3X,     N4X,
     2   N5X,     N6X,     N1Y,     N2Y,
     3   N3Y,     N4Y,     N5Y,     N6Y,
     4   N1Z,     N2Z,     N3Z,     N4Z,
     5   N5Z,     N6Z,     X1,      X2,
     6   X3,      X4,      X5,      X6,
     7   X7,      X8,      Y1,      Y2,
     8   Y3,      Y4,      Y5,      Y6,
     9   Y7,      Y8,      Z1,      Z2,
     A   Z3,      Z4,      Z5,      Z6,
     B   Z7,      Z8,      AREAM,   NEL)
        DO I=1,NEL
          DELTAX(I)=TWO*VOLN(I)*XIOFF(I)/SQRT(AREAM(I))
        ENDDO
      ELSE
        CALL SLEN(
     1   X1,      X2,      X3,      X4,
     2   Y1,      Y2,      Y3,      Y4,
     3   Z1,      Z2,      Z3,      Z4,
     4   1,       AREA,    AREAM,   NEL)
        CALL SLEN(
     1   X5,      X6,      X7,      X8,
     2   Y5,      Y6,      Y7,      Y8,
     3   Z5,      Z6,      Z7,      Z8,
     4   2,       AREA,    AREAM,   NEL)
        CALL SLEN(
     1   X1,      X2,      X6,      X5,
     2   Y1,      Y2,      Y6,      Y5,
     3   Z1,      Z2,      Z6,      Z5,
     4   3,       AREA,    AREAM,   NEL)
        CALL SLEN(
     1   X2,      X3,      X7,      X6,
     2   Y2,      Y3,      Y7,      Y6,
     3   Z2,      Z3,      Z7,      Z6,
     4   4,       AREA,    AREAM,   NEL)
        CALL SLEN(
     1   X3,      X4,      X8,      X7,
     2   Y3,      Y4,      Y8,      Y7,
     3   Z3,      Z4,      Z8,      Z7,
     4   5,       AREA,    AREAM,   NEL)
        CALL SLEN(
     1   X4,      X1,      X5,      X8,
     2   Y4,      Y1,      Y5,      Y8,
     3   Z4,      Z1,      Z5,      Z8,
     4   6,       AREA,    AREAM,   NEL)
        IF (IDTS6>0) THEN
          CALL SLDEGE(
     1   X1,      X2,      X3,      X4,
     2   X5,      X6,      X7,      X8,
     3   Y1,      Y2,      Y3,      Y4,
     4   Y5,      Y6,      Y7,      Y8,
     5   Z1,      Z2,      Z3,      Z4,
     6   Z5,      Z6,      Z7,      Z8,
     7   AREA,    AREAM,   VOLN,    NEL)
        ENDIF
        IF(MTN==5.OR.MTN==41)THEN
          DO I=1,NEL
            ATEST(I)=EM4*AREAM(I)
          ENDDO       
          DO I=1,NEL                            
            NFAC=0                                
            IF(AREA(I,1)<ATEST(I)) NFAC=NFAC+1 
            IF(AREA(I,2)<ATEST(I)) NFAC=NFAC+1 
            IF(AREA(I,3)<ATEST(I)) NFAC=NFAC+1 
            IF(AREA(I,4)<ATEST(I)) NFAC=NFAC+1 
            IF(AREA(I,5)<ATEST(I)) NFAC=NFAC+1 
            IF(AREA(I,6)<ATEST(I)) NFAC=NFAC+1 
            IF(NFAC>=3) XIOFF(I)=EP03           
          ENDDO                                          
       END IF
       DO  I=1,NEL
         DELTAX(I)=FOUR*VOLN(I)*XIOFF(I)/SQRT(AREAM(I))
       ENDDO      
      ENDIF
C-----------------------------------------------      
      RETURN
      END
