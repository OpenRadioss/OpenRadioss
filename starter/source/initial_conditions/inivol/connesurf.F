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
Chd|  CONNESURF                     source/initial_conditions/inivol/connesurf.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CONNESURF(NSEG, SURF_NODES ,KNOD2SURF,NNOD2SURF)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KNOD2SURF(*),NNOD2SURF,NSEG,SURF_NODES(NSEG,4)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N,IN(4)
C-----------------------------------------------
C-----------------------------------------------
C    Connect  segments of surf (just for intersection treatment)
C-----------------------------------------------
      DO I=1,NSEG
        IN(1) = SURF_NODES(I,1)
        IN(2) = SURF_NODES(I,2)
        IN(3) = SURF_NODES(I,3)
        IN(4) = SURF_NODES(I,4)
C
        DO K=1,4
          N = IN(K)
          KNOD2SURF(N) = KNOD2SURF(N) + 1
        END DO
      ENDDO
C
      DO I=1,NSEG
        IN(1) = SURF_NODES(I,1)
        IN(2) = SURF_NODES(I,2)
        IN(3) = SURF_NODES(I,3)
        IN(4) = SURF_NODES(I,4)
        DO K=1,4
          N = IN(K)
          NNOD2SURF = MAX(NNOD2SURF,KNOD2SURF(N))
        END DO
      ENDDO
!---
      RETURN
      END
