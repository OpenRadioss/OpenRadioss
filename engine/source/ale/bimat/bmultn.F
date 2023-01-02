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
Chd|  BMULTN                        source/ale/bimat/bmultn.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BMULTN(FILL,DFILL,IMS,IFILL,NODFT,NODLT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IFILL,NODFT,NODLT
      INTEGER IMS(NUMNOD,*)
      my_real
     .   FILL(NUMNOD,*), DFILL(NUMNOD,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER M, N
C-----------------------------------------------
      DO M=1,NMULT
        DO N=NODFT,NODLT
          IF(IMS(N,M)>0)THEN
            DFILL(N,M)=DT2*DFILL(N,M)/IMS(N,M)
          ELSE
            DFILL(N,M)=ZERO
          ENDIF
        ENDDO !next N
      ENDDO !next M

      DO M=1,NMULT
        DO N=NODFT,NODLT
          IF(IMS(N,M)/=0)THEN
            IMS(N,M) = 0
            FILL(N,M)= FILL(N,M)+DFILL(N,M)
            FILL(N,M)= MIN(ONE,FILL(N,M))
            FILL(N,M)= MAX(-ONE,FILL(N,M))
          ENDIF
        ENDDO !next N
      ENDDO !next M

      RETURN
      END
