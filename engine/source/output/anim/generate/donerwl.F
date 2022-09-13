Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  DONERWL                       source/output/anim/generate/donerwl.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE DONERWL(IRWL,NERWL,NPRW)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IRWL,NERWL,
     .        NPRW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N2, N3, N4, ITYP
C-----------------------------------------------
C seul le proc 0 effectue le traitement
      IF (ISPMD/=0) GOTO 100
C
         N2=IRWL+NRWALL
         N3=N2+NRWALL
         N4=N3+NRWALL
         ITYP= NPRW(N4)
         IF(IABS(ITYP)==1)THEN
            NERWL = NERWL + 1 + 1
         ELSEIF(ITYP==2)THEN
            NERWL = NERWL + 24 + 1
         ELSEIF(ITYP==3)THEN
            NERWL = NERWL + 216 + 1
         ELSEIF(ITYP==4)THEN
            NERWL = NERWL + 1 + 1
         ENDIF
C
 100  CONTINUE
      RETURN
      END
