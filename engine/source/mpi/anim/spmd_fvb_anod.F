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
Chd|  SPMD_FVB_ANOD                 source/mpi/anim/spmd_fvb_anod.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_ANOD()
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
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
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, PMAIN, J, ITAG, MSGOFF, STAT(MPI_STATUS_SIZE),
     .        IERR, LEN, NNS_ANIM, JJ,MSGOFF2
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RBUF
      REAL R4
C-----------------------------------------------
      DATA MSGOFF/7038/
      DATA MSGOFF2/7039/
C-----------------------------------------------
C
      DO I=1,NFVBAG
         PMAIN=FVSPMD(I)%PMAIN
         IF (ISPMD==0) THEN
            IF (ISPMD==PMAIN-1) THEN
               DO J=1,FVDATA(I)%NNS_ANIM
                  R4=FVDATA(I)%NOD_ANIM(1,J)
                  CALL WRITE_R_C(R4,1)
                  R4=FVDATA(I)%NOD_ANIM(2,J)
                  CALL WRITE_R_C(R4,1)
                  R4=FVDATA(I)%NOD_ANIM(3,J)
                  CALL WRITE_R_C(R4,1)
               ENDDO
            ELSE
               ITAG=MSGOFF
               CALL MPI_RECV(NNS_ANIM, 1, MPI_INTEGER,
     .                       IT_SPMD(PMAIN), ITAG, MPI_COMM_WORLD,
     .                       STAT, IERR)
C
               LEN=3*NNS_ANIM
               ALLOCATE(RBUF(LEN))
               ITAG=MSGOFF2
               CALL MPI_RECV(RBUF, LEN, REAL, IT_SPMD(PMAIN),
     .                       ITAG, MPI_COMM_WORLD, STAT, IERR)
C
               DO J=1,LEN
                  R4=RBUF(J)
                  CALL WRITE_R_C(R4,1)
               ENDDO
               DEALLOCATE(RBUF)
            ENDIF
         ELSE
            IF (ISPMD==PMAIN-1) THEN
               ITAG=MSGOFF
               CALL MPI_SEND(FVDATA(I)%NNS_ANIM, 1, MPI_INTEGER,
     .                       IT_SPMD(1), ITAG, MPI_COMM_WORLD, 
     .                       IERR)
C
               LEN=3*FVDATA(I)%NNS_ANIM
               ALLOCATE(RBUF(LEN))
               JJ=0
               DO J=1,FVDATA(I)%NNS_ANIM
                  JJ=JJ+1
                  RBUF(JJ)=FVDATA(I)%NOD_ANIM(1,J)
                  JJ=JJ+1
                  RBUF(JJ)=FVDATA(I)%NOD_ANIM(2,J)
                  JJ=JJ+1
                  RBUF(JJ)=FVDATA(I)%NOD_ANIM(3,J)
               ENDDO
               ITAG=MSGOFF2
               CALL MPI_SEND(RBUF, LEN, REAL, IT_SPMD(1),
     .                       ITAG, MPI_COMM_WORLD,  IERR)
C
               DEALLOCATE(RBUF)
            ENDIF
         ENDIF
      ENDDO         
C
#endif
      RETURN
      END
