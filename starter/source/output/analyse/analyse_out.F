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
C
C
Chd|====================================================================
Chd|  WRITC                         source/output/analyse/analyse_out.F
Chd|-- called by -----------
Chd|        WIOUT                         source/output/analyse/analyse_out.F
Chd|        WISTDO                        source/output/analyse/analyse_out.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE WRITC(UNIT,LENGTH,TABINT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENGTH, UNIT
      INTEGER TABINT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      CHARACTER*80 MESSAGE
      INTEGER I, LMESS
C
      MESSAGE = ' '
C
      LMESS = 0
C
      DO I=1, LENGTH
         LMESS = LMESS + 1
C
         IF (TABINT(I) == 10) THEN
            IF (LMESS > 1) THEN
               WRITE(UNIT,'(A)') MESSAGE(1:LMESS-1)
            ELSE
               WRITE(UNIT,'(A)') ' '
            ENDIF
            LMESS = 0
         ELSEIF ( LMESS ==80 ) THEN
            MESSAGE(LMESS:LMESS)=CHAR(TABINT(I))
            WRITE(UNIT,'(A)') MESSAGE(1:LMESS)
            LMESS = 0
         ELSE
            MESSAGE(LMESS:LMESS)=CHAR(TABINT(I))
         ENDIF
C
      ENDDO
C
      IF (LMESS > 0) THEN
         WRITE(UNIT,'(A)') MESSAGE(1:LMESS)
      ENDIF
C
      RETURN
      END
C
C
C
Chd|====================================================================
Chd|  WIOUT                         source/output/analyse/analyse_out.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITC                         source/output/analyse/analyse_out.F
Chd|====================================================================
      INTEGER FUNCTION WIOUT(LENGTH,TABINT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENGTH
      INTEGER TABINT(*)
C
      CALL WRITC(IOUT,LENGTH,TABINT)
      WIOUT = 0
      RETURN
C
      END
C
C
C
Chd|====================================================================
Chd|  WISTDO                        source/output/analyse/analyse_out.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        WRITC                         source/output/analyse/analyse_out.F
Chd|====================================================================
      INTEGER FUNCTION WISTDO(LENGTH,TABINT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LENGTH
      INTEGER TABINT(*)
C
      CALL WRITC(ISTDO,LENGTH,TABINT)
C
      WISTDO = 0
      RETURN
      END
C
C
C
