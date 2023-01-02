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
#include "hardware.inc"
Chd|====================================================================
Chd|  SPMD_DS_ISEND                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_ISEND(BUF, SIZE, ITAG, IDEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER BUF(*), SIZE, ITAG, IDEST
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IERR
C
      CALL MPI_SEND(BUF, SIZE, MPI_INTEGER, IT_SPMD(IDEST), ITAG,
     .              MPI_COMM_WORLD, IERR)
*      WRITE(*,*) 'Requete I envoyee - ITAG ',ITAG,' IERR ',IERR
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DS_RSEND                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_RSEND(BUF, SIZE, ITAG, IDEST)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZE, ITAG, IDEST
      my_real
     .        BUF(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IERR
C
      CALL MPI_SEND(BUF, SIZE, REAL, IT_SPMD(IDEST), ITAG,
     .              MPI_COMM_WORLD, IERR)
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DS_IRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_IRECV(BUF, SIZE, ITAG, IPROV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER BUF(*), SIZE, ITAG, IPROV
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IERR, ISTAT(MPI_STATUS_SIZE), LEN_STR, IERR_STR
      CHARACTER STR_ERROR*(MPI_MAX_ERROR_STRING)
C
      CALL MPI_RECV(BUF, SIZE, MPI_INTEGER, IT_SPMD(IPROV), ITAG,
     .              MPI_COMM_WORLD, ISTAT, IERR)
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DS_RRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_RRECV(BUF, SIZE, ITAG, IPROV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZE, ITAG, IPROV
      my_real
     .        BUF(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IERR, ISTAT(MPI_STATUS_SIZE)
C
      CALL MPI_RECV(BUF, SIZE, REAL, IT_SPMD(IPROV), ITAG,
     .              MPI_COMM_WORLD, ISTAT, IERR)
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DS_IEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_IEXCH(SBUF, RBUF, SIZE, MSGOFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
      INTEGER SIZE, SBUF(SIZE,*), RBUF(SIZE,*), MSGOFF
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, ITAG, REQ(2), IERR,
     .        TSTAT(MPI_STATUS_SIZE,2)
C
      DO I=1,NSPMD
         IF (ISPMD==I-1) THEN
            RBUF(1,I)=SBUF(1,I)
            RBUF(2,I)=SBUF(2,I)
         ELSE
            ITAG=MSGOFF + NSPMD*ISPMD + I
            CALL MPI_ISEND(SBUF(1,I), SIZE, MPI_INTEGER, IT_SPMD(I), 
     .                     ITAG, MPI_COMM_WORLD, REQ(1), IERR)
C
            ITAG=MSGOFF + NSPMD*(I-1) + ISPMD+1
            CALL MPI_IRECV(RBUF(1,I), SIZE, MPI_INTEGER, IT_SPMD(I), 
     .                     ITAG, MPI_COMM_WORLD, REQ(2), IERR)
C
            CALL MPI_WAITALL(2, REQ, TSTAT, IERR)
         ENDIF
      ENDDO
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DS_MEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_MEXCH(MATR, N   , IEXCH, REXCH, MSGOFF,
     .                         IADS, IADR, NN   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
      INTEGER N, NN, IEXCH(NN,*), MSGOFF, IADS(*), IADR(*)
      my_real
     .        MATR(N,*), REXCH(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IADC, LEN, ITAG(3), J, IR, IC, REQ(6),
     .        TSTAT(MPI_STATUS_SIZE,6), IERR, LENR
      INTEGER, DIMENSION(:), ALLOCATABLE :: IROW, ICOL
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: VAL
C
      DO I=1,NSPMD
         IF (ISPMD==I-1) THEN
            IADC=IADS(I)
            LEN=IADS(I+1)-IADS(I)
            DO J=1,LEN
               IR=IEXCH(IADC+J-1,1)
               IC=IEXCH(IADC+J-1,2)
               MATR(IR,IC)=MATR(IR,IC)+REXCH(IADC+J-1)
            ENDDO
         ELSE
C Reception
            LEN=IADR(I+1)-IADR(I)
            LENR=LEN
            ALLOCATE(IROW(LEN), ICOL(LEN), VAL(LEN))
            ITAG(1)=MSGOFF + NSPMD*3*(I-1) + ISPMD+1
            ITAG(2)=MSGOFF + NSPMD*3*(I-1) + NSPMD+ISPMD+1
            ITAG(3)=MSGOFF + NSPMD*3*(I-1) + 2*NSPMD+ISPMD+1
            CALL MPI_IRECV(IROW, LEN, MPI_INTEGER, IT_SPMD(I),
     .                     ITAG(1), MPI_COMM_WORLD, REQ(1), IERR)
            CALL MPI_IRECV(ICOL, LEN, MPI_INTEGER, IT_SPMD(I),
     .                     ITAG(2), MPI_COMM_WORLD, REQ(2), IERR)
            CALL MPI_IRECV(VAL, LEN, REAL, IT_SPMD(I),
     .                     ITAG(3), MPI_COMM_WORLD, REQ(3), IERR)
C Envoi
            IADC=IADS(I)
            LEN=IADS(I+1)-IADS(I)
            ITAG(1)=MSGOFF + NSPMD*3*ISPMD + I
            ITAG(2)=MSGOFF + NSPMD*3*ISPMD + NSPMD+I
            ITAG(3)=MSGOFF + NSPMD*3*ISPMD + 2*NSPMD+I
            CALL MPI_ISEND(IEXCH(IADC,1), LEN, MPI_INTEGER, IT_SPMD(I),
     .                     ITAG(1), MPI_COMM_WORLD, REQ(4), IERR)
            CALL MPI_ISEND(IEXCH(IADC,2), LEN, MPI_INTEGER, IT_SPMD(I),
     .                     ITAG(2), MPI_COMM_WORLD, REQ(5), IERR)
            CALL MPI_ISEND(REXCH(IADC), LEN, REAL, IT_SPMD(I),
     .                     ITAG(3), MPI_COMM_WORLD, REQ(6), IERR)
C
            CALL MPI_WAITALL(6, REQ, TSTAT, IERR)
C
            DO J=1,LENR
               IR=IROW(J)
               IC=ICOL(J)
               MATR(IR,IC)=MATR(IR,IC)+VAL(J)
            ENDDO
            DEALLOCATE(IROW, ICOL, VAL)
         ENDIF
      ENDDO
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DS_VEXCH                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_VEXCH(VECT, N   , IEXCH, REXCH, MSGOFF,
     .                         IADS, IADR, NN   , NV   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
      INTEGER N, IEXCH(*), MSGOFF, IADS(*), IADR(*), NN, NV
      my_real
     .        VECT(N,*), REXCH(NN,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, IADC, LEN, ITAG, J, K, IR, REQ(4),
     .        TSTAT(MPI_STATUS_SIZE, 4), IERR, II, LEN2, LENR
      INTEGER, DIMENSION(:), ALLOCATABLE :: IROW
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VAL, VALS
C
      DO I=1,NSPMD
         IF (ISPMD==I-1) THEN
            IADC=IADS(I)
            LEN=IADS(I+1)-IADS(I)
            DO J=1,NV
               DO K=1,LEN
                  IR=IEXCH(IADC+K-1)
                  VECT(IR,J)=VECT(IR,J)+REXCH(IADC+K-1,J)
               ENDDO
            ENDDO
         ELSE
C Reception
            LEN=IADR(I+1)-IADR(I)
            LENR=LEN
            II=0
            IF (LEN>0) THEN
               ALLOCATE(IROW(LEN), VAL(LEN,NV))
               ITAG=MSGOFF + NSPMD*2*(I-1) + ISPMD+1
               II=II+1
               CALL MPI_IRECV(IROW, LEN, MPI_INTEGER, IT_SPMD(I),
     .                        ITAG, MPI_COMM_WORLD, REQ(II), IERR)
               ITAG=MSGOFF + NSPMD*2*(I-1) + NSPMD + ISPMD+1
               LEN2=LEN*NV
               II=II+1
               CALL MPI_IRECV(VAL, LEN2, REAL, IT_SPMD(I),
     .                        ITAG, MPI_COMM_WORLD, REQ(II), IERR)
            ENDIF
C Envoi
            IADC=IADS(I)
            LEN=IADS(I+1)-IADS(I)
            IF (LEN>0) THEN
               ITAG=MSGOFF + NSPMD*2*ISPMD + I
               II=II+1
               CALL MPI_ISEND(IEXCH(IADC), LEN, MPI_INTEGER, IT_SPMD(I),
     .                        ITAG, MPI_COMM_WORLD, REQ(II), IERR)
               ALLOCATE(VALS(LEN,NV))
               DO J=1,NV
                  DO K=1,LEN
                     VALS(K,J)=REXCH(IADC+K-1,J)
                  ENDDO
               ENDDO
               ITAG=MSGOFF + NSPMD*2*ISPMD + NSPMD + I
               LEN2=LEN*NV
               II=II+1
               CALL MPI_ISEND(VALS, LEN2, REAL, IT_SPMD(I),
     .                        ITAG, MPI_COMM_WORLD, REQ(II), IERR)
            ENDIF
C
            IF (II>0) CALL MPI_WAITALL(II, REQ, TSTAT, IERR)
C
            DO J=1,NV
               DO K=1,LENR
                  IR=IROW(K)
                  VECT(IR,J)=VECT(IR,J)+VAL(K,J)
               ENDDO
            ENDDO
            IF (LEN>0) DEALLOCATE(VALS)
            IF (LENR>0) DEALLOCATE(IROW, VAL)
         ENDIF
      ENDDO
C
#endif
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_DS_VDESC                 source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_DS_VDESC(V     , NDEPL, VV, NDDLC, NLOC,
     .                         MSGOFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
      INTEGER NDEPL, NDDLC, NLOC, MSGOFF
      my_real
     .        V(NLOC,*), VV(NDDLC,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER ITAG, LEN, IERR, I, NPLOC, IPROV, 
     .        ISTAT(MPI_STATUS_SIZE), J, KK, IAD1, K
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VP
C
      IF (ISPMD/=0.AND.MOD(ISPMD,DSNCOL)==0) THEN
         ITAG=MSGOFF + ISPMD
         CALL MPI_SEND(NLOC, 1, MPI_INTEGER, IT_SPMD(1), ITAG,
     .                 MPI_COMM_WORLD, IERR)
C
         ITAG=MSGOFF + NSPMD + ISPMD
         LEN=NLOC*NDEPL
         CALL MPI_SEND(V, LEN, REAL, IT_SPMD(1), ITAG,
     .                 MPI_COMM_WORLD, IERR)
      ENDIF
C
      IF (ISPMD==0) THEN
         DO I=1,DSNROW
            IF (I==1) THEN
               NPLOC=NLOC
               ALLOCATE(VP(NPLOC,NDEPL))
               DO J=1,NDEPL
                  DO K=1,NPLOC
                     VP(K,J)=V(K,J)
                  ENDDO
               ENDDO
            ELSE
               IPROV=(I-1)*DSNCOL+1
               ITAG=MSGOFF + IPROV-1
               CALL MPI_RECV(NPLOC, 1, MPI_INTEGER, IT_SPMD(IPROV), 
     .                       ITAG, MPI_COMM_WORLD, ISTAT, IERR)
C
               ITAG=MSGOFF + NSPMD + IPROV-1
               ALLOCATE(VP(NPLOC,NDEPL))
               LEN=NPLOC*NDEPL
               CALL MPI_RECV(VP, LEN, REAL, IT_SPMD(IPROV), ITAG,
     .                       MPI_COMM_WORLD, ISTAT, IERR)
            ENDIF
C
            DO J=1,NDEPL
               KK=0
               IAD1=(I-1)*DSNBLOC
               DO K=1,NPLOC
                  IAD1=IAD1+1
                  KK=KK+1
                  IF (KK>DSNBLOC) THEN
                     IAD1=IAD1+DSNBLOC*(DSNROW-1)
                     KK=1
                  ENDIF
                  VV(IAD1,J)=VP(K,J)
               ENDDO
            ENDDO
            DEALLOCATE(VP)
         ENDDO
C
         DO I=2,NSPMD
            ITAG=MSGOFF + 2*NSPMD + I-1
            LEN=NDDLC*NDEPL
            CALL MPI_SEND(VV, LEN, REAL, IT_SPMD(I), ITAG,
     .                    MPI_COMM_WORLD, IERR)
         ENDDO  
      ELSE
         ITAG=MSGOFF + 2*NSPMD + ISPMD
         LEN=NDDLC*NDEPL
         CALL MPI_RECV(VV, LEN, REAL, IT_SPMD(1), ITAG,
     .                 MPI_COMM_WORLD, ISTAT, IERR)
      ENDIF                 
C
#endif
      RETURN
      END      
Chd|====================================================================
Chd|  SPMD_IWLG                     source/mpi/implicit/spmd_dsreso.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_IWLG(NDDLT  , NDDLG, NDDL, LSDDL, IW,
     .                     MSGOFF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
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
      INTEGER NDDLT, NDDLG, NDDL, LSDDL(*), IW(*), MSGOFF
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, ITAG(2,NDDLT), II, NDDLPM, IRQTAG, NDDLP(NSPMD-1),
     .        ISTAT(MPI_STATUS_SIZE), IERR, J, JJ, N
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: LDDLP
C
      IF (ISPMD==0) THEN
         DO I=1,NDDLT
            ITAG(1,I)=0
         ENDDO
C
         DO I=1,NDDL
            II=LSDDL(I)
            ITAG(1,II)=1
         ENDDO
C
         NDDLPM=0
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + I
            CALL MPI_RECV(NDDLP(I), 1, MPI_INTEGER, IT_SPMD(I+1),
     .                    IRQTAG, MPI_COMM_WORLD, ISTAT, IERR)
            NDDLPM=MAX(NDDLPM,NDDLP(I))            
         ENDDO
         ALLOCATE(LDDLP(NDDLPM,NSPMD-1))
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + NSPMD + I
            CALL MPI_RECV(
     .          LDDLP(1,I), NDDLP(I), MPI_INTEGER, IT_SPMD(I+1),
     .          IRQTAG, MPI_COMM_WORLD, ISTAT, IERR)
            DO J=1,NDDLP(I)
               JJ=LDDLP(J,I)
               ITAG(1,JJ)=1
            ENDDO
         ENDDO
C
         N=0
         DO I=1,NDDLT
            IF (ITAG(1,I)==1) THEN
               N=N+1
               ITAG(2,I)=N
            ENDIF
         ENDDO
         NDDLG=N
C
         DO I=1,NDDL
            II=LSDDL(I)
            IW(I)=ITAG(2,II)
         ENDDO
C
         DO I=1,NSPMD-1
            DO J=1,NDDLP(I)
               JJ=LDDLP(J,I)
               LDDLP(J,I)=ITAG(2,JJ)
            ENDDO
            IRQTAG=MSGOFF + 2*NSPMD + I
            CALL MPI_SEND(NDDLG, 1, MPI_INTEGER, IT_SPMD(I+1),
     .                    IRQTAG, MPI_COMM_WORLD,  IERR)
            IRQTAG=MSGOFF + 3*NSPMD + I
            CALL MPI_SEND(
     .          LDDLP(1,I), NDDLP(I), MPI_INTEGER, IT_SPMD(I+1),
     .          IRQTAG, MPI_COMM_WORLD,  IERR)
         ENDDO
         DEALLOCATE(LDDLP)
      ELSE
         IRQTAG=MSGOFF + ISPMD
         CALL MPI_SEND(NDDL, 1, MPI_INTEGER, IT_SPMD(1),
     .                 IRQTAG, MPI_COMM_WORLD,  IERR)
         IRQTAG=MSGOFF + NSPMD + ISPMD
         CALL MPI_SEND(LSDDL, NDDL, MPI_INTEGER, IT_SPMD(1),
     .                 IRQTAG, MPI_COMM_WORLD,  IERR)
         IRQTAG=MSGOFF + 2*NSPMD + ISPMD
         CALL MPI_RECV(NDDLG, 1, MPI_INTEGER, IT_SPMD(1),
     .                 IRQTAG, MPI_COMM_WORLD, ISTAT, IERR)
         IRQTAG=MSGOFF + 3*NSPMD + ISPMD
         CALL MPI_RECV(IW, NDDL, MPI_INTEGER, IT_SPMD(1),
     .                 IRQTAG, MPI_COMM_WORLD, ISTAT, IERR)
      ENDIF
C
#endif
      RETURN
      END

