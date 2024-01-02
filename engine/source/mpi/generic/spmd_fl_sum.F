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
Chd|  SPMD_FL_SUM                   source/mpi/generic/spmd_fl_sum.F
Chd|-- called by -----------
Chd|        BEMSOLVP                      source/fluid/bemsolvp.F       
Chd|        DAAACC                        source/fluid/daaacc.F         
Chd|        DAASOLVP                      source/fluid/daasolvp.F       
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_FL_SUM(LSUM, LEN, LSUMT)
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LEN
      my_real
     .        LSUM(*), LSUMT(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF, MSGOFF2,ITAG, REQ(2), TSTAT(MPI_STATUS_SIZE,2), IERR,
     .        I, II, REQ1(NSPMD-1), J,
     .        STAT1(MPI_STATUS_SIZE,NSPMD-1)
      my_real
     .        LSUMC(LEN)
C
      DATA MSGOFF/186/
      DATA MSGOFF2/187/
C
      IF (ISPMD/=0) THEN
         ITAG=MSGOFF
         CALL MPI_ISEND(LSUM, LEN, REAL, IT_SPMD(1), ITAG,
     .                  MPI_COMM_WORLD, REQ(1), IERR)
C
         ITAG=MSGOFF2
         CALL MPI_IRECV(LSUMT, LEN, REAL, IT_SPMD(1), ITAG,
     .                  MPI_COMM_WORLD, REQ(2), IERR)
C
         CALL MPI_WAITALL(2, REQ, TSTAT, IERR)
      ELSE
         DO I=1,LEN
            LSUMT(I)=LSUM(I)
         ENDDO
C
         DO I=1,NSPMD-1
            II=I+1
            ITAG=MSGOFF
            CALL MPI_IRECV(LSUMC, LEN, REAL, IT_SPMD(II), ITAG,
     .                     MPI_COMM_WORLD, REQ1(1), IERR)
            CALL MPI_WAIT(REQ1(1), STAT1(1,1), IERR)
C
            DO J=1,LEN
               LSUMT(J)=LSUMT(J)+LSUMC(J)
            ENDDO
         ENDDO
C
         DO I=1,NSPMD-1
            II=I+1
            ITAG=MSGOFF2
            CALL MPI_ISEND(LSUMT, LEN, REAL, IT_SPMD(II), ITAG,
     .                     MPI_COMM_WORLD, REQ1(I), IERR)
         ENDDO
C
         CALL MPI_WAITALL(NSPMD-1, REQ1, STAT1, IERR)
      ENDIF
C
#endif
      RETURN
      END
