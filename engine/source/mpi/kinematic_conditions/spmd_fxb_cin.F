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
Chd|  SPMD_FXB_CIN                  source/mpi/kinematic_conditions/spmd_fxb_cin.F
Chd|-- called by -----------
Chd|        FXBYVIT                       source/constraints/fxbody/fxbyvit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_FXB_CIN(FXBIPM, FXBVIT, FXBACC)
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
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*)
      my_real
     .        FXBVIT(*), FXBACC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER NFX, NMOD, NME, LEN, AVAR, PMAIN, II, I, ITAG, MSGOFF,
     .        REQ(NSPMD-1), STAT(MPI_STATUS_SIZE,NSPMD-1), IERR
      my_real
     .        , DIMENSION(:), ALLOCATABLE :: BUF
C
      DATA MSGOFF /198/
C
      DO NFX=1,NFXBODY
         NMOD=FXBIPM(4,NFX)
         NME=FXBIPM(17,NFX)
         LEN=2*(NME+NMOD)
         AVAR=FXBIPM(13,NFX)
         PMAIN=FXBIPM(39,NFX)
         ALLOCATE(BUF(LEN))
         IF (ISPMD==PMAIN) THEN
            II=0
            DO I=1,NME+NMOD
               BUF(I)=FXBVIT(I)
               BUF(NME+NMOD+I)=FXBACC(I)
            ENDDO
            DO I=1,NSPMD
               IF (ISPMD==I-1) CYCLE
               II=II+1
               ITAG=MSGOFF
               CALL MPI_ISEND(BUF, LEN, REAL, IT_SPMD(I), ITAG,
     .                        MPI_COMM_WORLD, REQ(II), IERR)
            ENDDO
C
            CALL MPI_WAITALL(NSPMD-1, REQ, STAT, IERR)
         ELSE
            ITAG=MSGOFF
            CALL MPI_IRECV(BUF, LEN, REAL, IT_SPMD(PMAIN+1), ITAG,
     .                     MPI_COMM_WORLD, REQ(1), IERR)
C
            CALL MPI_WAIT(REQ, STAT, IERR)
            DO I=1,NME+NMOD
               FXBVIT(I)=BUF(I)
               FXBACC(I)=BUF(NME+NMOD+I)
            ENDDO
         ENDIF
         DEALLOCATE(BUF)
      ENDDO
C
#endif
      RETURN
      END
C

