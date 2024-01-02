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
Chd|  CHKMSIN                       source/output/outfile/chkmsin.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CHKMSIN(NODFT,NODLT,ITAB,MS,IN,NEGMAS)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "comlock.inc"
#include      "com01_c.inc"
#include      "units_c.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODFT,NODLT,ITAB(*),NEGMAS
C     REAL
      my_real
     .   MS(*) ,IN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  N
C
C     NEGMAS=0
      DO N=NODFT,NODLT
        IF(MS(N)<ZERO)THEN
#include "lockon.inc"
          NEGMAS=NEGMAS+1
          WRITE(ISTDO,*) ' NEGATIVE MASS ON NODE ID=',ITAB(N)
          WRITE(IOUT,*)  ' NEGATIVE MASS ON NODE ID=',ITAB(N)
#include "lockoff.inc"
        ENDIF
      ENDDO
      IF(IRODDL/=0)THEN
        DO N=NODFT,NODLT
          IF(IN(N)<ZERO)THEN
#include "lockon.inc"
            NEGMAS=NEGMAS+1
            WRITE(ISTDO,*) ' NEGATIVE INERTIA ON NODE ID=',ITAB(N)
            WRITE(IOUT,*)  ' NEGATIVE INERTIA ON NODE ID=',ITAB(N)
#include "lockoff.inc"
          ENDIF
        ENDDO
      ENDIF
C
      RETURN
      END
