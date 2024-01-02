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
Chd|  DPARSRG                       source/output/anim/generate/dparsrg.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE DPARSRG(NSURG,NNWL,NODCUT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSURG,NNWL,NODCUT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ISRG, N1, II(4),
     .        I,J,K
C-----------------------------------------------
C seul le proc 0 effectue le traitement
      IF (ISPMD/=0) GOTO 100
C-----------------------------------------------
c      IF (IMACH/=3) THEN
c        N1 = NUMNOD + NODCUT + NSECT + NRWALL + NNWL
c      ELSE
        N1 = NUMNODG + NODCUT + NSECT + NRWALL + NNWL
c      ENDIF
      DO ISRG=1,NSURG
          DO I = 1,6
           DO J = 1,7
            DO K = 1,7
             II(1) = N1
             II(2) = N1 + 1
             II(3) = N1 + 9
             II(4) = N1 + 8
             CALL WRITE_I_C(II,4)
             N1 = N1 + 1
            ENDDO
            N1 = N1 + 1
           ENDDO
           N1 = N1 + 8
          ENDDO
      END DO
C
 100  CONTINUE
      RETURN
      END
