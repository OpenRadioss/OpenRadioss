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
Chd|  SPMD_FVB_ADIM                 source/mpi/anim/spmd_fvb_adim.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F     
Chd|====================================================================
      SUBROUTINE SPMD_FVB_ADIM(NFVTR, FVOFF , NFVNOD, NFVPART, NFVSUBS,
     .                         IDMAX, ITAB  , NODCUT, NNWL   , NNSRG  ,
     .                         NNSMD, NNSPHG)
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
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NFVTR, FVOFF(2,*), NFVNOD, NFVPART, NFVSUBS, IDMAX,
     .        ITAB(*), NODCUT, NNWL, NNSRG, NNSMD, NNSPHG
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IDMAXL, I, ITAG, MSGOFF, IDMAXP(NSPMD-1), 
     .        REQ(NSPMD-1), IERR, STAT(MPI_STATUS_SIZE,NSPMD-1),
     .        PMAIN, TAB(4),MSGTYP
      DATA MSGOFF/7037/
C
      IDMAXL=0
      DO I=1,NUMNOD
         IDMAXL=MAX(IDMAXL,ITAB(I))
      ENDDO
      IF (ISPMD==0) THEN
         DO I=1,NSPMD-1
            MSGTYP=MSGOFF
            CALL MPI_IRECV(IDMAXP(I), 1, MPI_INTEGER, IT_SPMD(I+1),
     .                     MSGTYP, MPI_COMM_WORLD, REQ(I), IERR)
         ENDDO
         CALL MPI_WAITALL(NSPMD-1, REQ, STAT, IERR)
C
         IDMAX=IDMAXL
         DO I=1,NSPMD-1
            IDMAX=MAX(IDMAX,IDMAXP(I))
         ENDDO
      ELSE
         MSGTYP=MSGOFF
         CALL MPI_ISEND(IDMAXL, 1, MPI_INTEGER, IT_SPMD(1),
     .                  MSGTYP, MPI_COMM_WORLD, REQ, IERR)
         CALL MPI_WAIT(REQ, STAT, IERR)
      ENDIF
C
      DO I=1,NFVBAG
         PMAIN=FVSPMD(I)%PMAIN
         IF (ISPMD==0) THEN
            IF (ISPMD==PMAIN-1) THEN
               NFVTR=NFVTR+FVDATA(I)%NNTR
               FVOFF(1,I)=NUMNODG+NODCUT+NSECT+NRWALL+NNWL
     .                   +NNSRG+NNSMD+NNSPHG+2*NUMELS16G+NFVNOD
               FVOFF(2,I)=IDMAX+NFVNOD
               NFVNOD=NFVNOD+FVDATA(I)%NNS_ANIM
               NFVPART=NFVPART+FVDATA(I)%NPOLH_ANIM
               NFVSUBS=NFVSUBS+1
            ELSE
               MSGTYP=MSGOFF
               CALL MPI_RECV(TAB, 3, MPI_INTEGER, IT_SPMD(PMAIN),
     .                       MSGTYP, MPI_COMM_WORLD, STAT, IERR)
C
               NFVTR=NFVTR+TAB(1)
               FVOFF(1,I)=NUMNODG+NODCUT+NSECT+NRWALL+NNWL
     .                   +NNSRG+NNSMD+NNSPHG+2*NUMELS16G+NFVNOD
               FVOFF(2,I)=IDMAX+NFVNOD
               NFVNOD=NFVNOD+TAB(2)
               NFVPART=NFVPART+TAB(3)
               NFVSUBS=NFVSUBS+1
            ENDIF
         ELSE
            IF (ISPMD==PMAIN-1) THEN
               MSGTYP=MSGOFF
               TAB(1)=FVDATA(I)%NNTR
               TAB(2)=FVDATA(I)%NNS_ANIM
               TAB(3)=FVDATA(I)%NPOLH_ANIM
               CALL MPI_SEND(TAB, 3, MPI_INTEGER, IT_SPMD(1),
     .                       MSGTYP, MPI_COMM_WORLD,  IERR)
            ENDIF
         ENDIF
      ENDDO
C
      IF (ISPMD==0) THEN
         TAB(1)=NFVTR
         TAB(2)=NFVNOD
         TAB(3)=NFVPART
         TAB(4)=NFVSUBS
      ENDIF
      CALL MPI_BCAST(TAB, 4, MPI_INTEGER, IT_SPMD(1),
     .               MPI_COMM_WORLD, IERR)
      IF (ISPMD/=0) THEN
         NFVTR=TAB(1)
         NFVNOD=TAB(2)
         NFVPART=TAB(3)
         NFVSUBS=TAB(4)
      ENDIF
C
#endif
      RETURN
      END
