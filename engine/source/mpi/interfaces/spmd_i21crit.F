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
C
Chd|====================================================================
Chd|  SPMD_I21CRIT                  source/mpi/interfaces/spmd_i21crit.F
Chd|-- called by -----------
Chd|        I21_ICRIT                     source/interfaces/intsort/i21_icrit.F
Chd|-- calls ---------------
Chd|        SPMD_RBCAST                   source/mpi/generic/spmd_rbcast.F
Chd|====================================================================
      SUBROUTINE SPMD_I21CRIT(GAPINF,VX,VY,VZ,DIST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "intstamp_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        GAPINF(*),VX(*),VY(*),VZ(*),DIST(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, N, MSGTYP, IERROR, LOC_PROC,
     .        SIZE
      my_real
     .        RBUF(5,NINTSTAMP,NSPMD),RRBUF(5,NINTSTAMP)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      LOC_PROC=ISPMD+1
      SIZE = 5*NINTSTAMP
C
      DO N=1,NINTSTAMP
        RRBUF(1,N) = GAPINF(N)
        RRBUF(2,N) = VX(N)
        RRBUF(3,N) = VY(N)
        RRBUF(4,N) = VZ(N)
        RRBUF(5,N) = DIST(N)
      END DO
C
      CALL MPI_GATHER(
     S    RRBUF         ,SIZE  ,REAL,
     R    RBUF          ,SIZE  ,REAL,IT_SPMD(1),
     G    MPI_COMM_WORLD,IERROR)
      IF(ISPMD==0) THEN
        DO N=1,NINTSTAMP
         DO I = 2, NSPMD
          IF(RBUF(1,N,I)<RBUF(1,N,1))THEN
            RBUF(1,N,1) = RBUF(1,N,I)
          END IF
          IF(RBUF(2,N,I)>RBUF(2,N,1))THEN
            RBUF(2,N,1) = RBUF(2,N,I)
          END IF
          IF(RBUF(3,N,I)>RBUF(3,N,1))THEN
            RBUF(3,N,1) = RBUF(3,N,I)
          END IF
          IF(RBUF(4,N,I)>RBUF(4,N,1))THEN
            RBUF(4,N,1) = RBUF(4,N,I)
          END IF
          IF(RBUF(5,N,I)<RBUF(5,N,1))THEN
            RBUF(5,N,1) = RBUF(5,N,I)
          END IF
         END DO
        END DO
      END IF
      CALL SPMD_RBCAST(RBUF,RBUF,SIZE,1,0,2)
C
      DO N=1,NINTSTAMP
        GAPINF(N)=RBUF(1,N,1)
        VX(N)    =RBUF(2,N,1)
        VY(N)    =RBUF(3,N,1)
        VZ(N)    =RBUF(4,N,1)
        DIST(N)  =RBUF(5,N,1)
      END DO
C
#endif
      RETURN
      END
