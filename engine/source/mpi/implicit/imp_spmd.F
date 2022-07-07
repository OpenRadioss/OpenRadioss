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
#include "hardware.inc"

#ifndef MPI
#define MPI_COMM_WORLD 0
#define MPI_INTEGER 0
#define MPI_SUM 0 
#define MPI_STATUS_SIZE 1
#endif
#if defined(MUMPS5)
Chd|====================================================================
Chd|  SPMD_MUMPS_FRONT              source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_FRONT(ITK  , RTK,    NKFRONT, NKFLOC, NKLOC,
     .                            NDDLG, IPRINT  )
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
#include      "units_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITK(2,*), NKFRONT, NKFLOC, NKLOC, NDDLG, IPRINT
      my_real
     .        RTK(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NKFP(NSPMD-1), IRQTAG, REQ1(NSPMD-1),
     .        TSTAT1(MPI_STATUS_SIZE,NSPMD-1), NKF_TOT, LEN, IR, JC,
     .        INDEX, J, K, NN, NKFMAX, NKF_NEW(NSPMD), PP, NMIN,
     .        PMIN, II, REQ2(2), TSTAT2(MPI_STATUS_SIZE,2),
     .        REQ3(3), 
     .        TSTAT3(MPI_STATUS_SIZE,3),REQ4(3),
     .        TSTAT4(MPI_STATUS_SIZE,3), IERR, NP, JJ, NKIP(NSPMD-1),
     .        SBUF(2), RBUF(2,NSPMD-1), NNZT, NDDLP(NSPMD-1),
     .        IADFIN, IAD, IAD0, ADDCM(NDDLG)
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITKF, IKFRONT, PKFRONT,
     .                                        KFMAP
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RTKF, RKFRONT
      INTEGER MSGOFF,MSGOFF2,MSGOFF3,MSGOFF4,MSGOFF5,MSGOFF6
      DATA MSGOFF/16000/,MSGOFF2/16062/
      DATA MSGOFF3/16000/,MSGOFF4/16062/
      DATA MSGOFF5/16003/,MSGOFF6/16064/



C
      ALLOCATE(ITKF(2,NKFRONT), RTKF(NKFRONT))
      DO I=1,NKFRONT
         ITKF(1,I)=ITK(1,NKLOC+I)
         ITKF(2,I)=ITK(2,NKLOC+I)
         RTKF(I)=RTK(NKLOC+I)
      ENDDO   
C
      IF (ISPMD.EQ.0) THEN
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF 
            CALL MPI_IRECV(RBUF(1,I), 2, MPI_INTEGER, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ1(I), IERR)
         ENDDO
         IF(NSPMD > 1) CALL MPI_WAITALL(NSPMD-1, REQ1, TSTAT1, IERR)
         DO I=1,NSPMD-1
            NKFP(I)=RBUF(1,I)
            NKIP(I)=RBUF(2,I)
         ENDDO
C
         NKF_TOT=NKFRONT
         DO I=1,NSPMD-1
            NKF_TOT=NKF_TOT+NKFP(I)
         ENDDO
         ALLOCATE(IKFRONT(3,NKF_TOT), RKFRONT(NKF_TOT), 
     .            PKFRONT(NSPMD+1,NKF_TOT))
C
C IKFRONT becomes a chained list
C
         DO I=1,NDDLG
          ADDCM(I)=0
         END DO
         IADFIN=0         
         DO I=1,NKFRONT
           IR=ITKF(1,I)
           JC=ITKF(2,I)
           IF(IR>NDDLG) STOP 1000
           IAD=ADDCM(IR)
           DO WHILE (IAD /= 0)
             IAD0=IAD
             IAD=IKFRONT(3,IAD)
           END DO
           IADFIN = IADFIN+1
           IKFRONT(1,IADFIN) = JC
           IKFRONT(2,IADFIN) = IR
           IKFRONT(3,IADFIN) = 0
           IF(ADDCM(IR) == 0)THEN
             ADDCM(IR)=IADFIN
           ELSE
             IKFRONT(3,IAD0)=IADFIN
           ENDIF
           RKFRONT(IADFIN)=RTKF(I)
           PKFRONT(1,IADFIN)=1
           PKFRONT(2,IADFIN)=1           
         END DO
C 
         NKF_TOT=NKFRONT
         DEALLOCATE(ITKF, RTKF)
C
         DO I=1,NSPMD-1
            ALLOCATE(ITKF(2,NKFP(I)), RTKF(NKFP(I)))
            IRQTAG=MSGOFF2
            LEN=2*NKFP(I)
            CALL MPI_IRECV(ITKF, LEN, MPI_INTEGER, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ2(1), IERR)
            IRQTAG=MSGOFF3 
            LEN=NKFP(I)
            CALL MPI_IRECV(RTKF, LEN, REAL, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ2(2), IERR)
            CALL MPI_WAITALL(2, REQ2, TSTAT2, IERR)
C
            DO J=1,NKFP(I)
               IR=ITKF(1,J)
               JC=ITKF(2,J)
               INDEX=0
C chained list
               IF(IR>NDDLG) STOP 2000
               IAD=ADDCM(IR)
               ! go through IKFRONT until JC is found at INDEX pos
               DO WHILE (IAD /= 0)
                 IF(IKFRONT(1,IAD) == JC)THEN
                   INDEX=IAD
                   IAD=0
                 ELSE
                   IAD0=IAD
                   IAD=IKFRONT(3,IAD)
                 END IF
               END DO
               IF(INDEX == 0) THEN
                 NKF_TOT = NKF_TOT+1
                 IKFRONT(1,NKF_TOT) = JC
                 IKFRONT(2,NKF_TOT) = IR
                 IKFRONT(3,NKF_TOT) = 0
                 IF(ADDCM(IR) == 0)THEN
                   ADDCM(IR)=NKF_TOT
                 ELSE
                   IKFRONT(3,IAD0)=NKF_TOT
                 ENDIF
                 RKFRONT(NKF_TOT)=RTKF(J)
                 PKFRONT(1,NKF_TOT)=1
                 PKFRONT(2,NKF_TOT)=I+1
               ELSE
                 RKFRONT(INDEX)=RKFRONT(INDEX)+RTKF(J)
                 NN=PKFRONT(1,INDEX)
                 NN=NN+1
                 PKFRONT(1,INDEX)=NN
                 PKFRONT(1+NN,INDEX)=I+1              
               END IF               
            ENDDO
            DEALLOCATE(ITKF, RTKF)
         ENDDO
C
         NNZT=NKLOC
         DO I=1,NSPMD-1
            NNZT=NNZT+NKIP(I)
         ENDDO
         NNZT=NNZT+NKF_TOT
         IF (ISPMD.EQ.0.AND.IPRINT.EQ.1) THEN
            WRITE(ISTDO,*)
            WRITE(ISTDO,'(A21,I10,A8,I10)') 
     .        ' MUMPS    DIM : NNZ =',NNZT,' NNZFR =',NKF_TOT
         ENDIF
C
C Affectation des termes de frontieres aux processeurs
         NKFMAX=NKFRONT
         DO I=1,NSPMD-1
            NKFMAX=MAX(NKFMAX,NKFP(I))
         ENDDO
C
         ALLOCATE(KFMAP(NSPMD,NKFMAX))
         DO I=1,NSPMD
            NKF_NEW(I)=0
         ENDDO
C PKFRONT(1,I) => number of proc 
C PKFRONT(2:NSPMD+1) => proc id 
C  KFMAP(PKFRONT(2,I),NKF_NEW(PKFRONT(2,I))) = I
C
C
         DO I=1,NKF_TOT
            IF (PKFRONT(1,I).EQ.1) THEN
               PP=PKFRONT(2,I)
               NN=NKF_NEW(PP)
               NN=NN+1
               KFMAP(PP,NN)=I
               NKF_NEW(PP)=NN
            ELSE
               NP=PKFRONT(1,I)
               PP=PKFRONT(2,I)
               NMIN=NKF_NEW(PP)
               PMIN=PP
               DO J=2,NP
                  PP=PKFRONT(1+J,I)
                  IF (NKF_NEW(PP).LT.NMIN) THEN
                     NMIN=NKF_NEW(PP)
                     PMIN=PP
                  ENDIF
               ENDDO
               NN=NKF_NEW(PMIN)
               NN=NN+1
               KFMAP(PMIN,NN)=I
               NKF_NEW(PMIN)=NN
            ENDIF
         ENDDO
C
         IF (ISPMD.EQ.0.AND.IPRINT.EQ.1) THEN
            WRITE(ISTDO,*)
            DO I=1,NSPMD
               IF (I.EQ.1) THEN
                  WRITE(ISTDO,'(A6,I5,5X,A5,I10,A8,I10)') 
     .                 ' PROC=',I,'NNZ =',NKLOC+NKF_NEW(1),
     .                 ' NNZFR =',NKF_NEW(1)
               ELSE
                  WRITE(ISTDO,'(A6,I5,5X,A5,I10,A8,I10)') 
     .                 ' PROC=',I,'NNZ =',NKIP(I-1)+NKF_NEW(I),
     .                 ' NNZFR =',NKF_NEW(I)
               ENDIF
            ENDDO
         ENDIF
C
         NKFLOC=NKF_NEW(1)
         DO I=1,NKFLOC
            II=KFMAP(1,I)
            ITK(1,NKLOC+I)=IKFRONT(1,II)
            ITK(2,NKLOC+I)=IKFRONT(2,II)
            RTK(NKLOC+I)=RKFRONT(II)
         ENDDO
C
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF4 
            CALL MPI_ISEND(NKF_NEW(I+1), 1, MPI_INTEGER, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ3(1), IERR)
C
            ALLOCATE(ITKF(2,NKF_NEW(I+1)), RTKF(NKF_NEW(I+1)))
            DO J=1,NKF_NEW(I+1)
               JJ=KFMAP(I+1,J)
               ITKF(1,J)=IKFRONT(1,JJ)
               ITKF(2,J)=IKFRONT(2,JJ)
               RTKF(J)=RKFRONT(JJ)
            ENDDO
            LEN=2*NKF_NEW(I+1)
            IRQTAG=MSGOFF5 
            CALL MPI_ISEND(ITKF, LEN, MPI_INTEGER, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD,
     .                     REQ3(2), IERR)
            LEN=NKF_NEW(I+1)
            IRQTAG=MSGOFF6
            CALL MPI_ISEND(RTKF, LEN, REAL, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD,
     .                     REQ3(3), IERR)
C
            CALL MPI_WAITALL(3, REQ3, TSTAT3, IERR)
C
            DEALLOCATE(ITKF, RTKF)
         ENDDO
C
         DEALLOCATE(IKFRONT, RKFRONT, PKFRONT)
      ELSE
         IRQTAG=MSGOFF 
         SBUF(1)=NKFRONT
         SBUF(2)=NKLOC
         CALL MPI_ISEND(SBUF, 2, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ4(1), IERR)
         CALL MPI_WAIT(REQ4, TSTAT4, IERR)
         LEN=2*NKFRONT
         IRQTAG=MSGOFF2
         CALL MPI_ISEND(ITKF, LEN, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ4(2), IERR)
         LEN=NKFRONT
         IRQTAG=MSGOFF3 
         CALL MPI_ISEND(RTKF, LEN, REAL, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ4(3), IERR)

         CALL MPI_WAITALL(3, REQ4, TSTAT4, IERR)
         DEALLOCATE(ITKF, RTKF)
C
         IRQTAG=MSGOFF4 
         CALL MPI_IRECV(NKFLOC, 1, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ4, IERR)
         CALL MPI_WAIT(REQ4, TSTAT4, IERR)
C
         ALLOCATE(ITKF(2,NKFLOC), RTKF(NKFLOC))
         LEN=2*NKFLOC
         IRQTAG=MSGOFF5 
         CALL MPI_IRECV(ITKF, LEN, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ4(1), IERR)
         LEN=NKFLOC
         IRQTAG=MSGOFF6 
         CALL MPI_IRECV(RTKF, LEN, REAL, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ4(2), IERR)
         CALL MPI_WAITALL(2, REQ4, TSTAT4, IERR)
C
         DO I=1,NKFLOC
            ITK(1,NKLOC+I)=ITKF(1,I)
            ITK(2,NKLOC+I)=ITKF(2,I)
            RTK(NKLOC+I)=RTKF(I)
         ENDDO
         DEALLOCATE(ITKF, RTKF)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MUMPS_COUNT              source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_COUNT(NZLOC, NZP, NNZ)
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
      INTEGER NZLOC, NZP(NSPMD-1), NNZ
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IRQTAG, MSGOFF, REQ(NSPMD-1),
     .        TSTAT(MPI_STATUS_SIZE,NSPMD-1), IERR
      DATA MSGOFF/16001/
C
      NNZ=0
      IF (ISPMD.EQ.0) THEN
         NNZ=NZLOC
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF 
            CALL MPI_IRECV(NZP(I), 1, MPI_INTEGER, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ(I), IERR)
         ENDDO

         IF(NSPMD > 1) CALL MPI_WAITALL(NSPMD-1, REQ, TSTAT, IERR)
         DO I=1,NSPMD-1
            NNZ=NNZ+NZP(I)
         ENDDO
      ELSE
         IRQTAG=MSGOFF 
         CALL MPI_ISEND(NZLOC, 1, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ, IERR)
         CALL MPI_WAIT(REQ, TSTAT, IERR)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MUMPS_GATH               source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_GATH(ITK, RTK, NZLOC, A, IRN, 
     .                           JCN, NZP)
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
      INTEGER ITK(2,*), NZLOC, IRN(*), JCN(*), NZP(*)
      my_real
     .        RTK(*), A(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER NNZ, I, IRQTAG, MSGOFF,MSGOFF2,
     .        LEN, REQ(2), IERR,
     .        TSTAT(MPI_STATUS_SIZE,2), J
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ITKP
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RTKP
      DATA MSGOFF/16002/
      DATA MSGOFF2/16055/

C
      IF (ISPMD.EQ.0) THEN
         NNZ=0
         DO I=1,NZLOC
            NNZ=NNZ+1
            IRN(NNZ)=ITK(1,I)
            JCN(NNZ)=ITK(2,I)
            A(NNZ)=RTK(I)
         ENDDO
C
         DO I=1,NSPMD-1
            ALLOCATE(ITKP(2,NZP(I)), RTKP(NZP(I)))
            IRQTAG=MSGOFF 
            LEN=2*NZP(I)
            CALL MPI_IRECV(ITKP, LEN, MPI_INTEGER, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ(1), IERR)
            IRQTAG=MSGOFF2
            LEN=NZP(I)
            CALL MPI_IRECV(RTKP, LEN, REAL, IT_SPMD(I+1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ(2), IERR)

            CALL MPI_WAITALL(2, REQ, TSTAT, IERR)
C
            DO J=1,NZP(I)
               NNZ=NNZ+1
               IRN(NNZ)=ITKP(1,J)
               JCN(NNZ)=ITKP(2,J)
               A(NNZ)=RTKP(J)
            ENDDO
            DEALLOCATE(ITKP, RTKP)
         ENDDO
      ELSE
         IRQTAG=MSGOFF
         LEN=2*NZLOC
         CALL MPI_ISEND(ITK, LEN, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ(1), IERR)
         IRQTAG=MSGOFF2
         LEN=NZLOC
         CALL MPI_ISEND(RTK, LEN, REAL, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ(2), IERR)
         CALL MPI_WAITALL(2, REQ, TSTAT, IERR)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MUMPS_INI                source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|        LAG_MULT_SDP                  source/tools/lagmul/lag_mult_solv.F
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_INI(MUMPS_PAR, SYM)
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
#include      "dmumps_struc.h"
#include      "com01_c.inc"
#include      "task_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SYM
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
C
#ifdef MPI
      MUMPS_PAR%COMM = MPI_COMM_WORLD
#else 
      MUMPS_PAR%COMM = 0
#endif
      MUMPS_PAR%JOB = -1
      MUMPS_PAR%SYM = SYM
      MUMPS_PAR%PAR = 1
c     CALL STARTIME(96,1)
      CALL DMUMPS(MUMPS_PAR)
c     CALL STOPTIME(96,1)

C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MUMPS_DEAL               source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DEALLOCM_IMP                  source/implicit/imp_solv.F    
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|        LAG_MULT_SDP                  source/tools/lagmul/lag_mult_solv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_DEAL(MUMPS_PAR)
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
#include      "dmumps_struc.h"
#include      "com01_c.inc"
#include      "task_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
C
      IF (ASSOCIATED(MUMPS_PAR%A))   DEALLOCATE(MUMPS_PAR%A)
      IF (ASSOCIATED(MUMPS_PAR%IRN)) DEALLOCATE(MUMPS_PAR%IRN)
      IF (ASSOCIATED(MUMPS_PAR%JCN)) DEALLOCATE(MUMPS_PAR%JCN)
      IF (ASSOCIATED(MUMPS_PAR%A_LOC))   DEALLOCATE(MUMPS_PAR%A_LOC)
      IF (ASSOCIATED(MUMPS_PAR%IRN_LOC)) DEALLOCATE(MUMPS_PAR%IRN_LOC)
      IF (ASSOCIATED(MUMPS_PAR%JCN_LOC)) DEALLOCATE(MUMPS_PAR%JCN_LOC)
      IF (ASSOCIATED(MUMPS_PAR%RHS)) DEALLOCATE(MUMPS_PAR%RHS)
      IF (ASSOCIATED(MUMPS_PAR%A)) THEN
         DEALLOCATE(MUMPS_PAR%A)
      ENDIF
      MUMPS_PAR%JOB=-2
      CALL DMUMPS(MUMPS_PAR)

C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MUMPS_RHS                source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_MUMPS2                    source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_RHS(V    , CDDLP, RHS, NDDL, ISENS,
     .                          NDDLG)
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
      INTEGER CDDLP(*), NDDL, ISENS, NDDLG
      my_real
     .        V(*), RHS(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IRQTAG, MSGOFF, II, REQ(NSPMD-1), 
     .        TSTAT(MPI_STATUS_SIZE,NSPMD-1), IERR,
     .        J, JJ
      my_real
     .        VG(NDDLG)
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: VP
      DATA MSGOFF/16003/
C
      IF (ISPMD.EQ.0) THEN
C
         IF (ISENS.EQ.1) THEN
            ALLOCATE(VP(NDDLG,NSPMD-1))
C Gather des forces
            DO I=1,NDDLG
               RHS(I)=ZERO
            ENDDO
            DO I=1,NDDL
               II=CDDLP(I)
               RHS(II)=V(I)
            ENDDO
C
            DO I=1,NSPMD-1
               IRQTAG=MSGOFF 
               CALL MPI_IRECV(VP(1,I), NDDLG, REAL, IT_SPMD(I+1),
     .                        IRQTAG, MPI_COMM_WORLD, REQ(I), IERR)
            ENDDO
            IF(NSPMD > 1) CALL MPI_WAITALL(NSPMD-1, REQ, TSTAT, IERR)
            DO I=1,NSPMD-1            
               DO J=1,NDDLG             
                  RHS(J)=RHS(J)+VP(J,I) 
               ENDDO                    
            ENDDO
C
            DEALLOCATE(VP)
C
         ELSEIF (ISENS.EQ.2) THEN
C Scatter des deplacements
            DO I=1,NDDL
               II=CDDLP(I)
               V(I)=RHS(II)
            ENDDO
C
            DO I=1,NSPMD-1
               IRQTAG=MSGOFF 
               CALL MPI_ISEND(RHS, NDDLG, REAL, IT_SPMD(I+1),
     .                        IRQTAG, MPI_COMM_WORLD, REQ(I), IERR)
            ENDDO
            IF(NSPMD > 1) CALL MPI_WAITALL(NSPMD-1, REQ, TSTAT, IERR)
         ENDIF
      ELSE
         IF (ISENS.EQ.1) THEN
            DO I=1,NDDLG
               VG(I)=ZERO
            ENDDO
            DO I=1,NDDL
               II=CDDLP(I)
               VG(II)=V(I)
            ENDDO
C
            IRQTAG=MSGOFF 
            CALL MPI_ISEND(VG, NDDLG, REAL, IT_SPMD(1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ, IERR)
            CALL MPI_WAIT(REQ, TSTAT, IERR)
         ELSEIF (ISENS.EQ.2) THEN
            IRQTAG=MSGOFF 
            CALL MPI_IRECV(VG, NDDLG, REAL, IT_SPMD(1),
     .                     IRQTAG, MPI_COMM_WORLD, REQ, IERR)
            CALL MPI_WAIT(REQ, TSTAT, IERR)
C
            DO I=1,NDDL
               II=CDDLP(I)
               V(I)=VG(II)
            ENDDO
         ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MUMPS_EXEC               source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_MUMPS2                    source/implicit/imp_mumps.F   
Chd|        LAG_MULT_SDP                  source/tools/lagmul/lag_mult_solv.F
Chd|-- calls ---------------
Chd|        IMP_ERRMUMPS                  source/implicit/imp_solv.F    
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_EXEC(MUMPS_PAR, ITASK)
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
#include "timeri_c.inc"
#include "dmumps_struc.h"
#include "impl1_c.inc"
#include "task_c.inc"
#include "com01_c.inc"
#include "units_c.inc"

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITASK
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER AVAIL_MEM,ESTIM_MEM
      INTEGER ORDERING_METHOD,METIS,PORD,AUTOMATIC 

! Only the testing here, initializations are removed into IMP_MUMPS1

      IF (ITASK.EQ.1) THEN
        METIS = 5
        PORD = 4

C Analyse
         MUMPS_PAR%JOB=1
C        CALL STARTIME(97,1)


         CALL DMUMPS(MUMPS_PAR)
C        CALL STOPTIME(97,1)
         ORDERING_METHOD =  MUMPS_PAR%INFOG(7)
         IF(MUMPS_PAR%INFOG(1) < 0) THEN
           IF(ISPMD ==0 ) THEN
              WRITE(IOUT,*) 'Warning: MUMPS Error in Analysis. Retry'
           ENDIF
C If there is an error during the analysis, then we retry with
C Another ordering method
           IF(ORDERING_METHOD /= PORD) THEN
             ORDERING_METHOD = PORD
           ELSE 
             ORDERING_METHOD = METIS
           ENDIF
           MUMPS_PAR%JOB=1
           MUMPS_PAR%ICNTL(7) = ORDERING_METHOD 
C          CALL STARTIME(97,1)
           CALL DMUMPS(MUMPS_PAR)
C          CALL STOPTIME(97,1)
         ENDIF

         IF( NSPMD == 1) NSPMD_PER_NODE = 1
C Workspace size
         AVAIL_MEM = INT(8.0d0 * LMEMV /(10.0d0 * NSPMD_PER_NODE ))
         IF(MUMPS_PAR%ICNTL(22) == 0 ) THEN
C In-core :
C INFOG(16) is the estimation already increased by 20pc. 
C We increase again by 20pc 
           ESTIM_MEM = INT(MUMPS_PAR%INFOG(16) * 1.2d0)
         ELSE 
C Out-of-core
           ESTIM_MEM = INT(MUMPS_PAR%INFOG(26) * 1.2d0)
         ENDIF
         MUMPS_PAR%ICNTL(23) = MIN(AVAIL_MEM,ESTIM_MEM)

C Factorisation
         MUMPS_PAR%JOB=2
C        CALL STARTIME(98,1)
         CALL DMUMPS(MUMPS_PAR)
C        CALL STOPTIME(98,1)
                                                            
C In-Core:Trying to recover from error 
         IF(MUMPS_PAR%INFOG(1) == -8) THEN
C if internal workspace is too small (IS)
           IF(ISPMD ==0 ) THEN
             WRITE(IOUT,*) 'Warning: MUMPS workspace too small. Retry'
           ENDIF
           MUMPS_PAR%ICNTL(14) = MUMPS_PAR%ICNTL(14) * 2
           MUMPS_PAR%JOB=2
C          CALL STARTIME(98,1)
           CALL DMUMPS(MUMPS_PAR)
C          CALL STOPTIME(98,1)

         ELSEIF(MUMPS_PAR%INFOG(1)==-9 .OR. MUMPS_PAR%INFOG(1)==-11) THEN
C if internal workspace is too small (S)
           IF(ISPMD ==0 ) THEN
             WRITE(IOUT,*) 'Warning: MUMPS workspace too small. Retry'
           ENDIF
  
           AVAIL_MEM = INT(9.5d0 * LMEMV /(10.0d0 * NSPMD_PER_NODE ))
           ESTIM_MEM = AVAIL_MEM
           MUMPS_PAR%ICNTL(23) = AVAIL_MEM
           MUMPS_PAR%JOB=2
c          CALL STARTIME(98,1)
           CALL DMUMPS(MUMPS_PAR)
c          CALL STOPTIME(98,1)

         ELSEIF(MUMPS_PAR%INFOG(1) == -13) THEN
C if internal workspace is too big: an allocation failed
           IF(ISPMD ==0 ) THEN
             WRITE(IOUT,*) 'Warning: MUMPS workspace too large. Retry'
           ENDIF
           AVAIL_MEM = INT(AVAIL_MEM * 8.0d0 / 10.0d0)
           ESTIM_MEM = INT(ESTIM_MEM * 8.0d0 / 10.0d0)
           MUMPS_PAR%ICNTL(23) = MIN(AVAIL_MEM,ESTIM_MEM)
           MUMPS_PAR%JOB=2
c          CALL STARTIME(98,1)
           CALL DMUMPS(MUMPS_PAR)
c          CALL STOPTIME(98,1)

         ELSEIF(MUMPS_PAR%INFOG(1)<0) THEN
C Try to deal with other errors                                      
           IF(ISPMD ==0 ) THEN
             WRITE(IOUT,*) 'Warning: MUMPS error. Retry'
           ENDIF
           AVAIL_MEM = INT(9.5d0 * LMEMV /(10.0d0 * NSPMD_PER_NODE ))
           ESTIM_MEM = AVAIL_MEM
           MUMPS_PAR%ICNTL(23) = AVAIL_MEM
           IF(ORDERING_METHOD /= PORD) THEN
             ORDERING_METHOD = PORD
           ELSE 
             ORDERING_METHOD = METIS
           ENDIF
           MUMPS_PAR%ICNTL(7) = ORDERING_METHOD 
           MUMPS_PAR%ICNTL(13) = 1        
           MUMPS_PAR%JOB=1
           CALL DMUMPS(MUMPS_PAR)
           MUMPS_PAR%JOB=2
c          CALL STARTIME(98,1)
           CALL DMUMPS(MUMPS_PAR)
c          CALL STOPTIME(98,1)
         ENDIF ! Recovering from error

       IF (MUMPS_PAR%INFOG(1)<0) THEN
c         WRITE(IOUT,*) 'Warning: failed to solve linear system '
c        WRITE(IOUT,*) 'MUMPS error code :',MUMPS_PAR%INFOG(1)
         CALL IMP_ERRMUMPS(MUMPS_PAR%INFOG(1))

       ENDIF



      ELSEIF (ITASK.EQ.2) THEN
C Resolution
         MUMPS_PAR%JOB=3
c        CALL STARTIME(99,1)
         CALL DMUMPS(MUMPS_PAR) 
c        CALL STOPTIME(99,1)

      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MUMPS_FLUSH              source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MUMPS_FLUSH(MUMPS_PAR)
C This routine write the centralized matrix and RHS into files
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
#include      "dmumps_struc.h"
#include      "com01_c.inc"
#include      "impl1_c.inc"
#include      "units_c.inc" 

* debug
#include      "task_c.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(DMUMPS_STRUC) MUMPS_PAR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      CHARACTER (len=255) file_mat,file_rhs,file_icntl
c     Uncomment the following lines to print the matrix into the matrix
c     Matrix Market format.
      IF(IMUMPSD .EQ. 2 .AND. ISPMD .EQ. 0) THEN
C If centralized matrix on zero
        OPEN(unit=21,file="matrix",action="write",status="replace",
     . form='unformatted')

        OPEN(unit=22,file="rhs",action="write",status="replace",
     . form='unformatted')

        OPEN(unit=23,file="icntl",action="write",status="replace",
     . form='unformatted')

        WRITE(21) MUMPS_PAR%N,MUMPS_PAR%NZ
        DO I = 1,MUMPS_PAR%NZ
        WRITE(21) MUMPS_PAR%IRN(I),MUMPS_PAR%JCN(I),MUMPS_PAR%A(I)
        END DO
        WRITE(22) MUMPS_PAR%N
        DO I = 1,MUMPS_PAR%N
        WRITE(22)  MUMPS_PAR%RHS(I)
        END DO
        WRITE(23) MUMPS_PAR%ICNTL
        CLOSE(21)
        CLOSE(22)
        CLOSE(23)
      ELSE                          

        WRITE(file_mat,"(A4,I4.4)") "mat_",ISPMD
        WRITE(file_rhs,"(A4,I4.4)") "rhs_",ISPMD
        WRITE(file_icntl,"(A4,I4.4)") "opt_",ISPMD

        OPEN(unit=21,file=file_mat,action="write",status="replace",
     . form='unformatted')

        OPEN(unit=22,file=file_rhs,action="write",status="replace",
     . form='unformatted')

        OPEN(unit=23,file=file_icntl,action="write",status="replace",
     . form='unformatted')


        WRITE(21) MUMPS_PAR%N,MUMPS_PAR%NZ,MUMPS_PAR%NZ_LOC
        DO I = 1,MUMPS_PAR%NZ_LOC
        WRITE(21) MUMPS_PAR%IRN_LOC(I),MUMPS_PAR%JCN_LOC(I),
     .  MUMPS_PAR%A_LOC(I)
        END DO
        IF( ISPMD .EQ. 0 ) THEN
          WRITE(22) MUMPS_PAR%N              
          DO I = 1,MUMPS_PAR%N
            WRITE(22)  MUMPS_PAR%RHS(I)
          END DO
        ENDIF
        WRITE(23) MUMPS_PAR%ICNTL
        CLOSE(21)
        CLOSE(22)
        CLOSE(23)

      ENDIF
      RETURN
      END
#endif

#if defined(MPI) && defined(MUMPS5)


Chd|====================================================================
Chd|  SPMD_SUM_S                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DYNA_IVFAC                    source/implicit/imp_dyna.F    
Chd|        IMP_CHECK0                    source/implicit/imp_solv.F    
Chd|        IMP_CHECM0                    source/implicit/imp_solv.F    
Chd|        IMP_COMPABP                   source/implicit/imp_solv.F    
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|        IND_FR_K0                     source/mpi/implicit/imp_fri.F 
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        NDDLI_FRB                     source/mpi/implicit/imp_fri.F 
Chd|        PRODUT_H                      source/implicit/produt_v.F    
Chd|        PRODUT_HP                     source/implicit/produt_v.F    
Chd|        PRODUT_U0                     source/implicit/produt_v.F    
Chd|        PRODUT_UHP0                   source/implicit/produt_v.F    
Chd|        PRODUT_V                      source/implicit/produt_v.F    
Chd|        PRODUT_W                      source/implicit/produt_v.F    
Chd|        SMS_PRODUT_H                  source/ams/sms_proj.F         
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SUM_S(S)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      my_real
     .        S
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      my_real
     .        RBUF(NSPMD),SI
      DATA MSGOFF/16004/
      DATA MSGOFF2/16005/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C---try
C---try      SI=S
C---try      CALL MPI_REDUCE(SI,S,1,REAL,MPI_SUM,0,MPI_COMM_WORLD,IERROR)
C---try      CALL MPI_BCAST(S,1,REAL,0,MPI_COMM_WORLD,IERROR)
      LOC_PROC = ISPMD + 1
      SIZ=1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(I),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          S = S + RBUF(I)
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

        MSGTYP = MSGOFF2 
        CALL MPI_RECV(S,SIZ,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_SUM_S2                   source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SUM_S2(S,LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
     .        S(LEN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ, IDEB,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      my_real
     .        RBUF(NSPMD*LEN),SI
      DATA MSGOFF/16006/
      DATA MSGOFF2/16007/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C---try
C---try      SI=S
C---try      CALL MPI_REDUCE(SI,S,1,REAL,MPI_SUM,0,MPI_COMM_WORLD,IERROR)
C---try      CALL MPI_BCAST(S,1,REAL,0,MPI_COMM_WORLD,IERROR)
      LOC_PROC = ISPMD + 1
      SIZ=LEN
      IF(ISPMD.EQ.0) THEN
       IDEB = SIZ+1
       DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
          IDEB=IDEB+SIZ
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          IDEB=INDEX*SIZ
          DO I = 1, SIZ
            S(I) = S(I) + RBUF(IDEB+I)
          END DO
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

        MSGTYP = MSGOFF2 
        CALL MPI_RECV(S,SIZ,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MAX_S                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        GET_MAX                       source/implicit/nl_solv.F     
Chd|        LECIMPL                       source/input/lectur.F         
Chd|        SMS_CHECK                     source/ams/sms_fsa_inv.F      
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MAX_S(S)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      my_real
     .        S
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      my_real
     .        RBUF(NSPMD),SI
      DATA MSGOFF/16008/,MSGOFF2/16009/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ=1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(I),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          S = MAX(S,RBUF(I))
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

        MSGTYP = MSGOFF2 
        CALL MPI_RECV(S,SIZ,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MIN_S                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_INTDT                     source/implicit/imp_int_k.F   
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|        SPB_IEREF3                    source/implicit/imp_solv.F    
Chd|        SPB_IEREF_BC                  source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MIN_S(S)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      my_real
     .        S
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      my_real
     .        RBUF(NSPMD),SI
      DATA MSGOFF/16009/
      DATA MSGOFF2/16010/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ=1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(I),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          S = MIN(S,RBUF(I))
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2
          CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(S,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

        MSGTYP = MSGOFF2 
        CALL MPI_RECV(S,SIZ,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MAX_I                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DIM_KINMAX                    source/implicit/ind_glob_k.F  
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        IMP_CHECK0                    source/implicit/imp_solv.F    
Chd|        IMP_CHECM0                    source/implicit/imp_solv.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_COMPABP                   source/implicit/imp_solv.F    
Chd|        IMP_GLOB_KHP                  source/implicit/imp_glob_k.F  
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        RBE2T1                        source/constraints/general/rbe2/rbe2f.F
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        SMS_DIAG_RBE2                 source/ams/sms_rbe2.F         
Chd|        SMS_RBE_CNDS                  source/ams/sms_rbe2.F         
Chd|        SMS_RBE_PREC                  source/ams/sms_rbe2.F         
Chd|        SPBRM_PRE                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MAX_I(N)
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
      INTEGER  N
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      INTEGER
     .        RBUF(NSPMD),SI
      DATA MSGOFF/16011/,MSGOFF2/16012/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ=1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO L = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          N = MAX(N,RBUF(I))
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(N,SIZ,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(N,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

        MSGTYP = MSGOFF2 
        CALL MPI_RECV(N,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C

      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MIN_I                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MIN_I(N)
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
      INTEGER  N
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,L,MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      INTEGER
     .        RBUF(NSPMD),SI
      DATA MSGOFF/16013/,MSGOFF2/16014/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ=1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO L = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          N = MIN(N,RBUF(I))
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(N,SIZ,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(N,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)

        MSGTYP = MSGOFF2 
        CALL MPI_RECV(N,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_INF_G                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|        PR_INFOK                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_INF_G(
     1   NDDL0    ,NZZK0   ,NDDL     ,NZZK     ,NNMAX    ,
     1   NDDL0P   ,NZZK0P  ,NDDLP    ,NZZKP    ,NNMAXP   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "impl1_c.inc" 
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL,NDDL0,NZZK,NZZK0,NNMAX,
     .        NDDLP(*),NDDL0P(*),NZZKP(*),NZZK0P(*),NNMAXP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,IBUF(2),
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      INTEGER
     .        RBUF(9,NSPMD),SI
      DATA MSGOFF/16015/
      DATA MSGOFF2/16016/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ=9
       RBUF(1,1) = NDDL0
       RBUF(2,1) = NZZK0
       RBUF(3,1) = NDDL
       RBUF(4,1) = NZZK
       RBUF(5,1) = NNMAX
       RBUF(6,1) = NDDLFR-2*NDDLFRB/3
       RBUF(7,1) = NZKFR
       RBUF(8,1) = LEN_V-2*NDDLFRB1/3
       RBUF(9,1) = LEN_K-LEN_V
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(1,I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
          NDDL0P(1)=RBUF(1,1)
          NZZK0P(1)=RBUF(2,1)
          NDDLP(1)=RBUF(3,1)
          NZZKP(1)=RBUF(4,1)
          NNMAXP(1)=RBUF(5,1)
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          NDDL0P(I)=RBUF(1,I)
          NZZK0P(I)=RBUF(2,I)
          NDDLP(I)=RBUF(3,I)
          NZZKP(I)=RBUF(4,I)
          NNMAXP(I)=RBUF(5,I)
          RBUF(1,1) = RBUF(1,1) + RBUF(1,I)
          RBUF(2,1) = RBUF(2,1) + RBUF(2,I)
          RBUF(3,1) = RBUF(3,1) + RBUF(3,I)
          RBUF(4,1) = RBUF(4,1) + RBUF(4,I)
          RBUF(5,1) = MAX(RBUF(5,1),RBUF(5,I))
          RBUF(6,1) = RBUF(6,1) + RBUF(6,I)
          RBUF(7,1) = RBUF(7,1) + RBUF(7,I)
          RBUF(8,1) = RBUF(8,1) + RBUF(8,I)
          RBUF(9,1) = RBUF(9,1) + RBUF(9,I)
        END DO
C
        NDDL0 = RBUF(1,1)-NDDLFRB
        NZZK0 = RBUF(2,1)-RBUF(7,1)/2
        NDDL = RBUF(3,1)-NDDLFRB1
        NZZK = RBUF(4,1)-RBUF(9,1)/2
        NNMAX = RBUF(5,1)
        IBUF(1) = NDDL
        IBUF(2) = NZZK
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(IBUF ,2,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
C
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(RBUF,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        MSGTYP = MSGOFF2
        CALL MPI_RECV(IBUF,2,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
         NDDL = IBUF(1)
         NZZK = IBUF(2)
      END IF
       NDDL_G = NDDL
       NNZK_G = NZZK
      IF (L_LIM.EQ.0) L_LIM = NDDL_G
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_SUMF_V                   source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        EXT_RHS                       source/implicit/upd_glob_k.F  
Chd|        GET_FEXT                      source/implicit/imp_solv.F    
Chd|        IMP_CHECK0                    source/implicit/imp_solv.F    
Chd|        IMP_FRFV                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRI                       source/mpi/implicit/imp_fri.F 
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        MAV_LTGH                      source/implicit/produt_v.F    
Chd|        MAV_LTH                       source/implicit/produt_v.F    
Chd|        MAV_LTH0                      source/implicit/produt_v.F    
Chd|        MAV_LTP                       source/implicit/produt_v.F    
Chd|        MMV_LH                        source/implicit/produt_v.F    
Chd|        MMV_LTH                       source/implicit/produt_v.F    
Chd|        PREC_SOLVGH                   source/implicit/prec_solv.F   
Chd|        PREC_SOLVH                    source/implicit/prec_solv.F   
Chd|        PREC_SOLVP                    source/implicit/prec_solv.F   
Chd|        RER02                         source/implicit/upd_glob_k.F  
Chd|        UPD_RHS                       source/implicit/upd_glob_k.F  
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_SUMF_V(V   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        V(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,ND,ID,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(LEN_V), SBUF(LEN_V)
      DATA MSGOFF/16017/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (NDDLFR.LE.0) RETURN
C
      LOC_PROC = ISPMD + 1
C
      L=1
      DO I=1,NSPMD
        SIZ = ND_FR(I)
        IF(SIZ.GT.0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L+ ND_FR(I)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      L = 0
      DO I=1,NSPMD
        IF(ND_FR(I).GT.0)THEN
          DO J=1,ND_FR(I)
           ID = J + L
           ND=IFR2K(ID)
           SBUF(ID) = V(ND)
          ENDDO
          L = L +ND_FR(I) 
        ENDIF
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      L = 1
      DO I=1,NSPMD
        SIZ = ND_FR(I)
        IF(SIZ.GT.0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
         L = L +ND_FR(I) 
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      L = 0
      DO I=1,NSPMD
        IF(ND_FR(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          DO J=1,ND_FR(I)
           ID = J + L
           ND=IFR2K(ID)
           V(ND) = V(ND) + RBUF(ID) 
          ENDDO
          L = L +ND_FR(I) 
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(ND_FR(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_SUMFC_V                  source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_PCGH                      source/implicit/imp_pcg.F     
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_SUMFC_V(VGAT,VSCA,INDEX,LCOM)
C specific communication with compacted V
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOM, INDEX(LCOM)
      my_real
     .        VGAT(LCOM), VSCA(LCOM)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,ND,ID,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DATA MSGOFF/16018/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (NDDLFR.LE.0) RETURN
C
      LOC_PROC = ISPMD + 1
C
      L=1
      DO I=1,NSPMD
        SIZ = ND_FR(I)
        IF(SIZ.GT.0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      VSCA(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
      END DO
C
      L = 1
      DO I=1,NSPMD
        SIZ = ND_FR(I)
        IF(SIZ.GT.0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      VGAT(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          L = L + SIZ
        ENDIF        
      ENDDO
C
      DO I = 1, NSPMD
        IF(ND_FR(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
C Compactage VSCAT sur CPU
C
      DO I = 1, LCOM
        L=INDEX(I)
        IF(L /= 0)THEN
          VSCA(L)=VSCA(L)+VSCA(I)
        END IF
      END DO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_SUMF_K                   source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        LIN_SOLVH1                    source/implicit/lin_solv.F    
Chd|        LIN_SOLVIH2                   source/implicit/lin_solv.F    
Chd|        UPD_ASPC                      source/constraints/general/bcs/bc_imp0.F
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_SUMF_K(DIAG_K   ,L_K     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .        DIAG_K(*),L_K(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ(NSPMD),J,K,L,ND,ID,JD,
     .        STATUS(MPI_STATUS_SIZE),IAD,JAD,IAD2,
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(LEN_K), SBUF(LEN_K)
      DATA MSGOFF/16019/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (NDDLFR.LE.0) RETURN
C
      LOC_PROC = ISPMD + 1
C
      IAD=0
      L=1
      DO I=1,NSPMD
        SIZ(I) = ND_FR(I)+IADFR(ND_FR(I)+IAD+1)-IADFR(IAD+1)
        IF(SIZ(I).GT.0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     .      RBUF(L),SIZ(I),REAL,IT_SPMD(I),MSGTYP,
     .      MPI_COMM_WORLD,REQ_R(I),IERROR)
         L=L+SIZ(I)
        ENDIF
        IAD=IAD+ND_FR(I)+1
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      IAD=0
      L=1
      IAD2=0
      JAD=0
       DO I=1,NSPMD
        IF(SIZ(I).GT.0)THEN
          DO J=1,ND_FR(I)
           ND=IFR2K(J+IAD2)
           SBUF(L)= DIAG_K(ND)   
           L = L + 1 
           ID = J + IAD
           DO K=IADFR(ID),IADFR(ID+1)-1
             JD=JFR2K(K+JAD)
             SBUF(L) = L_K(JD)
             L = L + 1
           ENDDO
          ENDDO
          IAD2 =IAD2 + ND_FR(I)
          JAD =JAD +IADFR(IAD+ND_FR(I)+1)-IADFR(IAD+1)
        ENDIF
        IAD =IAD + ND_FR(I)+1
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      L=1
      DO I=1,NSPMD
        IF(SIZ(I).GT.0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ(I),REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          L = L + SIZ(I)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      IAD=0
      L=1
      IAD2=0
      JAD=0
       DO I=1,NSPMD
        IF(SIZ(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          DO J=1,ND_FR(I)
           ND=IFR2K(J+IAD2)
           DIAG_K(ND)=DIAG_K(ND)+RBUF(L)  
           L = L + 1 
           ID = J + IAD
           DO K=IADFR(ID),IADFR(ID+1)-1
             JD=JFR2K(K+JAD)
             L_K(JD)=L_K(JD)+RBUF(L)
             L = L + 1
           ENDDO
          ENDDO
          IAD2 =IAD2 + ND_FR(I)
          JAD =JAD +IADFR(IAD+ND_FR(I)+1)-IADFR(IAD+1)
        ENDIF
        IAD =IAD + ND_FR(I)+1
       ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(SIZ(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_INIS                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_FRSL                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_INIS(IAD_S,IAD_R)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER IAD_R(*),IAD_S(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER
     .        RBUF(NSPMD), SBUF(NSPMD)
      DATA MSGOFF/16020/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C----creer IAD_R (taille de recevoir)a partir de IAD_S------
      LOC_PROC = ISPMD + 1
C------recive--
        DO I=1,NSPMD
          MSGTYP = MSGOFF 
         IF(I.NE.LOC_PROC)THEN
          CALL MPI_IRECV(
     S      RBUF(I),1  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
         ENDIF
       END DO
C
C   echange messages
C--------------------------------------------------------------------
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          MSGTYP = MSGOFF 
          SBUF(I) = IAD_S(I+1)-IAD_S(I)
          CALL MPI_ISEND(
     S      SBUF(I),1,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      IAD_R(1)=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          IAD_R(I+1) = IAD_R(I)+RBUF(I)
        ELSE
          IAD_R(I+1) = IAD_R(I)
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(I.NE.LOC_PROC)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_INISL                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_FRSL                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_INISL(NBINTC,INBSL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NBINTC,INBSL(NBINTC,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD),SIZ,L,J,IAD
      INTEGER
     .        RBUF(NSPMD*NBINTC), SBUF(NSPMD*NBINTC)
      DATA MSGOFF/16021/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C----creer IAD_R (taille de recevoir)a partir de IAD_S------
      LOC_PROC = ISPMD + 1
      SIZ = NBINTC 
C------recive--
      L = 1
        DO I=1,NSPMD
          MSGTYP = MSGOFF 
         IF(I.NE.LOC_PROC)THEN
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
         ENDIF
       END DO
C
C   echange messages
C--------------------------------------------------------------------
      L = 1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          MSGTYP = MSGOFF 
          DO J = 1, NBINTC
           SBUF(L+J-1) = INBSL(J,I)
          ENDDO
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
          L = L + SIZ
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      L = 0
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          DO J = 1, NBINTC
           INBSL(J,I) = RBUF(L+J) 
          ENDDO
          L = L + SIZ
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(I.NE.LOC_PROC)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_IFC1                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKI                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFC1(SSIZE ,RSIZE,KSS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER SSIZE  ,RSIZE
      my_real
     .        KSS(6,RSIZE)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,SIZE,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(7*RSIZE), SBUF(7*SSIZE)
      DATA MSGOFF/16022/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------com. de numero nodes SECONDARYs et diag_Kss-----
C au l'appel, SSIZE=IAD_SREM(NSPMD+1)-1 
C             RSIZE=IAD_SL(NSPMD+1)-1 
C------recive--
        SIZE=7
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_SL(I+1)-IAD_SL(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
          SBUF(L)=FR_SREM(J)
          SBUF(L+1)=FFI(1,J)
          SBUF(L+2)=FFI(2,J)
          SBUF(L+3)=FFI(3,J)
          SBUF(L+4)=DFI(1,J)
          SBUF(L+5)=DFI(2,J)
          SBUF(L+6)=DFI(3,J)
          L = L+SIZE
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_SREM(I+1)-IAD_SREM(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SL(I+1).GT.IAD_SL(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SL(I),IAD_SL(I+1)-1
          ISL(J) = INT(RBUF(L))
          KSS(1,J) = RBUF(L+1)
          KSS(2,J) = RBUF(L+2)
          KSS(3,J) = RBUF(L+3)
          KSS(4,J) = RBUF(L+4)
          KSS(5,J) = RBUF(L+5)
          KSS(6,J) = RBUF(L+6)
          L = L+SIZE
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SREM(I+1)-IAD_SREM(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_IFCD                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFCD(D_IMP,SSIZE  ,RSIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER SSIZE  ,RSIZE
      my_real
     .        D_IMP(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,SIZE,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(3*RSIZE), SBUF(3*SSIZE)
      DATA MSGOFF/16023/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ com. de D nodes SECONDARYs -----
C------recive--
        SIZE=3
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_SREM(I+1)-IAD_SREM(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SL(I),IAD_SL(I+1)-1
          NOD=ISL(J)
          SBUF(L)  =D_IMP(1,NOD)
          SBUF(L+1)=D_IMP(2,NOD)
          SBUF(L+2)=D_IMP(3,NOD)
          L = L+SIZE
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_SL(I+1)-IAD_SL(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SREM(I+1).GT.IAD_SREM(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
          DFI(1,J) = RBUF(L)
          DFI(2,J) = RBUF(L+1)
          DFI(3,J) = RBUF(L+2)
          L = L+SIZE
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SL(I+1)-IAD_SL(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_IFCF                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|        INT_MATVP                     source/implicit/imp_int_k.F   
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFCF(F_IMP,SSIZE  ,RSIZE)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER SSIZE  ,RSIZE
      my_real
     .        F_IMP(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,SIZE,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(3*RSIZE), SBUF(3*SSIZE)
      DATA MSGOFF/16024/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ comm. de F nodes SECONDARYs -----
C------recive--
        SIZE=3
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_SL(I+1)-IAD_SL(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
          SBUF(L)  =FFI(1,J)
          SBUF(L+1)=FFI(2,J)
          SBUF(L+2)=FFI(3,J)
          L = L+SIZE
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_SREM(I+1)-IAD_SREM(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SL(I+1).GT.IAD_SL(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SL(I),IAD_SL(I+1)-1
          NOD=ISL(J)
          F_IMP(1,NOD) = F_IMP(1,NOD) + RBUF(L)
          F_IMP(2,NOD) = F_IMP(2,NOD) + RBUF(L+1)
          F_IMP(3,NOD) = F_IMP(3,NOD) + RBUF(L+2)
          L = L+SIZE
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SREM(I+1)-IAD_SREM(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_NROW                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        NDDLI_FRB                     source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_NROW(NROW,FR_NROW,IAD_ELEM,TSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NROW(*),FR_NROW(*),IAD_ELEM(2,*),TSIZE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,NOD,LOC_PROC,IERROR,
     .        STATUS(MPI_STATUS_SIZE),SIZ,
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        I,J,K,L
      INTEGER
     .        RBUF(TSIZE), SBUF(TSIZE)
      DATA MSGOFF/16025/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      DO I=1,NSPMD
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(1,I)
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      DO I=1,NSPMD
       DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
        SBUF(J) = NROW(J)
       ENDDO
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      DO I=1,NSPMD
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(1,I)
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      DO I=1,NSPMD
       SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
       IF(SIZ.GT.0)THEN
        CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
         FR_NROW(J) = RBUF(J)
        ENDDO
       ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF((IAD_ELEM(1,I+1)-IAD_ELEM(1,I)).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_ICOL                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DIM_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|        INI_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        REORDER_A                     source/implicit/ind_glob_k.F  
Chd|====================================================================
      SUBROUTINE SPMD_ICOL(
     1   IAD_S    ,IAD_R    ,NNMAX    ,ICOL     ,NROW     ,
     2   FR_NROW  ,IAD_ELEM ,FR_ELEM  ,SSIZE    ,RSIZE    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NNMAX  
      INTEGER IAD_R(*),IAD_S(*),FR_NROW(*),NROW(*),
     .        IAD_ELEM(2,*),FR_ELEM(*),ICOL(NNMAX,*),
     .        SSIZE ,RSIZE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,NOD,LOC_PROC,IERROR,
     .        STATUS(MPI_STATUS_SIZE),SIZ,
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        I,J,K,L,N1,NROWT
      INTEGER
     .        RBUF(RSIZE), SBUF(SSIZE)
      DATA MSGOFF/16026/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      DO I=1,NSPMD
        SIZ = IAD_R(I+1)-IAD_R(I)
        IF(SIZ.GT.0)THEN
          L = IAD_R(I)
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      DO I=1,NSPMD
       SIZ = IAD_S(I+1)-IAD_S(I)
       IF(SIZ.GT.0)THEN
        L = IAD_S(I)-1
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          DO K=1,NROW(J)
           SBUF(K+L) = ICOL(K,J)
          ENDDO
          L=L+NROW(J)
        ENDDO
       ENDIF
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      DO I=1,NSPMD
        SIZ = IAD_S(I+1)-IAD_S(I)
        IF(SIZ.GT.0)THEN
          L = IAD_S(I)
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      DO I=1,NSPMD
       SIZ = IAD_R(I+1)-IAD_R(I)
       IF(SIZ.GT.0)THEN
        CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        L=IAD_R(I)-1
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NROWT=NROW(J)
          DO K=1,FR_NROW(J)
           N1=RBUF(L+K)
           CALL REORDER_A(NROWT,ICOL(1,J),N1) 
          ENDDO
          L=L+FR_NROW(J)
          FR_NROW(J)=NROWT
        ENDDO
       ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_S(I+1)-IAD_S(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_I2D                      source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DIM_NDOF_D                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_I2D(NDOF,FR_ELEM,IAD_ELEM,TSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NDOF(*),FR_ELEM(*),IAD_ELEM(*),TSIZE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,NOD,LOC_PROC,IERROR,
     .        STATUS(MPI_STATUS_SIZE),SIZ,
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        I,J,K,L
      INTEGER
     .        RBUF(TSIZE), SBUF(TSIZE)
      DATA MSGOFF/16027/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      DO I=1,NSPMD
        SIZ = IAD_ELEM(I+1)-IAD_ELEM(I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(I)
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      DO I=1,NSPMD
       DO J=IAD_ELEM(I),IAD_ELEM(I+1)-1
        NOD = FR_ELEM(J)
        SBUF(J) = NDOF(NOD)
       ENDDO
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      DO I=1,NSPMD
        SIZ = IAD_ELEM(I+1)-IAD_ELEM(I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(I)
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      DO I=1,NSPMD
       SIZ = IAD_ELEM(I+1)-IAD_ELEM(I)
       IF(SIZ.GT.0)THEN
        CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        DO J=IAD_ELEM(I),IAD_ELEM(I+1)-1
         NOD = FR_ELEM(J)
         NDOF(NOD) = MAX(NDOF(NOD),RBUF(J))
        ENDDO
       ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF((IAD_ELEM(I+1)-IAD_ELEM(I)).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_SUMF_A                   source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_DYKV                      source/implicit/imp_dyna.F    
Chd|        IMP_DYKV0                     source/implicit/imp_dyna.F    
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SUMF_A(A ,AR,IAD_ELEM,FR_ELEM,SIZE,LR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER FR_ELEM(*),IAD_ELEM(2,*),SIZE,LR
      my_real
     .     A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(LR*SIZE), SBUF(LR*SIZE)
      DATA MSGOFF/16028/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ com. de D nodes SECONDARYs -----
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L)  =A(1,NOD)
              SBUF(L+1)=A(2,NOD)
              SBUF(L+2)=A(3,NOD)
              SBUF(L+3) =  AR(1,NOD)
              SBUF(L+4) =  AR(2,NOD)
              SBUF(L+5) =  AR(3,NOD)
              L = L+SIZE
            ENDDO
          ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SBUF(L  ) =  A(1,NOD)
              SBUF(L+1) =  A(2,NOD)
              SBUF(L+2) =  A(3,NOD)
              L = L + SIZE
            ENDDO
          ENDIF
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_ELEM(1,I+1)-IAD_ELEM(1,I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_ELEM(1,I+1).GT.IAD_ELEM(1,I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          IF(IRODDL.NE.0) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              A(1,NOD)=A(1,NOD)+RBUF(L) 
              A(2,NOD)=A(2,NOD)+RBUF(L+1) 
              A(3,NOD)=A(3,NOD)+RBUF(L+2) 
              AR(1,NOD)=AR(1,NOD)+RBUF(L+3) 
              AR(2,NOD)=AR(2,NOD)+RBUF(L+4) 
              AR(3,NOD)=AR(3,NOD)+RBUF(L+5) 
              L = L+SIZE
            ENDDO
          ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              A(1,NOD)=A(1,NOD)+RBUF(L) 
              A(2,NOD)=A(2,NOD)+RBUF(L+1) 
              A(3,NOD)=A(3,NOD)+RBUF(L+2) 
              L = L + SIZE
            ENDDO
          ENDIF
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_NDOF                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DIM_GLOB_K                    source/implicit/ind_glob_k.F  
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_NDOF(NDOF,FR_ELEM,IAD_ELEM,TSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NDOF(*),FR_ELEM(*),IAD_ELEM(2,*),TSIZE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,NOD,LOC_PROC,IERROR,
     .        STATUS(MPI_STATUS_SIZE),SIZ,
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        I,J,K,L
      INTEGER
     .        RBUF(TSIZE), SBUF(TSIZE)
      DATA MSGOFF/16029/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      DO I=1,NSPMD
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(1,I)
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      DO I=1,NSPMD
       DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
        NOD = FR_ELEM(J)
        SBUF(J) = NDOF(NOD)
       ENDDO
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      DO I=1,NSPMD
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(1,I)
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      DO I=1,NSPMD
       SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
       IF(SIZ.GT.0)THEN
        CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
         NOD = FR_ELEM(J)
         NDOF(NOD) = MAX(NDOF(NOD),RBUF(J))
        ENDDO
       ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF((IAD_ELEM(1,I+1)-IAD_ELEM(1,I)).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END


C-----------------------------------------------
Chd|====================================================================
Chd|  SPMD_CDDL                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_MUMPS1                    source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_CDDL(NDDL , NODGLOB, IDDL , NDOF , CDDLP,
     .                     INLOC, IKC    , NDDLG, NDDLP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, NODGLOB(*), IDDL(*), NDOF(*), CDDLP(*), 
     .        INLOC(*), IKC(*), NDDLG, NDDLP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, ITAG(6,NUMNODG), NKC, N, ND, ID, TDDL(2,NDDL),
     .        NDDLGL, NDDLC, LEN, IRQTAG, MSGOFF, REQ(2), IERR,
     .        STAT(MPI_STATUS_SIZE,2), NN, JJ,MSGOFF2,MSGOFF3
      INTEGER, DIMENSION(:), ALLOCATABLE :: CDDLPC
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TDDLC
      DATA MSGOFF/16030/,MSGOFF2/16031/,MSGOFF3/16056/

C
      DO I=1,NUMNODG
         DO J=1,6
            ITAG(J,I)=0
         ENDDO
      ENDDO
C
      NKC=0
      DO N=1,NUMNOD
         I=INLOC(N)
         DO J=1,NDOF(I)
            ND=IDDL(I)+J
            ID=ND-NKC
            IF (IKC(ND).LT.1) THEN
               TDDL(1,ID)=NODGLOB(I)
               TDDL(2,ID)=J
            ELSE
               NKC=NKC+1
            ENDIF
         ENDDO
      ENDDO
C
      IF (ISPMD.EQ.0) THEN
         NDDLGL=0
         DO I=1,NSPMD
            NDDLC=NDDLP(I)
            ALLOCATE(TDDLC(2,NDDLC), CDDLPC(NDDLC))
            IF (I.EQ.1) THEN
               DO J=1,NDDLC
                  TDDLC(1,J)=TDDL(1,J)
                  TDDLC(2,J)=TDDL(2,J)
               ENDDO
            ELSE
               LEN=2*NDDLC
               IRQTAG=MSGOFF 
               CALL MPI_IRECV(TDDLC, LEN, MPI_INTEGER, IT_SPMD(I),
     .                        IRQTAG, MPI_COMM_WORLD, REQ, IERR)
               CALL MPI_WAIT(REQ, STAT, IERR)
            ENDIF
C
            DO J=1,NDDLC
               NN=TDDLC(1,J)
               ID=TDDLC(2,J)
               JJ=ITAG(ID,NN)
               IF (JJ.EQ.0) THEN
                  NDDLGL=NDDLGL+1
                  ITAG(ID,NN)=NDDLGL
                  JJ=NDDLGL
               ENDIF
               CDDLPC(J)=JJ
            ENDDO
C
            IF (I.EQ.1) THEN
               DO J=1,NDDLC
                  CDDLP(J)=CDDLPC(J)
               ENDDO
            ELSE
               IRQTAG=MSGOFF3 
               CALL MPI_ISEND(CDDLPC, NDDLC, MPI_INTEGER, IT_SPMD(I),
     .                        IRQTAG, MPI_COMM_WORLD, REQ, IERR)
               CALL MPI_WAIT(REQ, STAT, IERR)
            ENDIF
C
            DEALLOCATE(TDDLC, CDDLPC)
         ENDDO
         NDDLG=NDDLGL
Cqz+4    
         DO I = 2, NSPMD
          IRQTAG=MSGOFF2 
          CALL MPI_SEND(NDDLG,1,MPI_INTEGER,IT_SPMD(I),
     .                   IRQTAG,MPI_COMM_WORLD,IERR)
         END DO
      ELSE
         LEN=2*NDDL
         IRQTAG=MSGOFF 
         CALL MPI_ISEND(TDDL, LEN, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ(1), IERR)
C
         IRQTAG=MSGOFF3 
         CALL MPI_IRECV(CDDLP, NDDL, MPI_INTEGER, IT_SPMD(1),
     .                  IRQTAG, MPI_COMM_WORLD, REQ(2), IERR)
C

         CALL MPI_WAITALL(2, REQ, STAT, IERR)
Cqz+3    
         IRQTAG = MSGOFF2 
         CALL MPI_RECV(NDDLG,1,MPI_INTEGER,IT_SPMD(1),IRQTAG,
     .                MPI_COMM_WORLD,STAT,IERR )
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_VCHGRID                  source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_VCHGRID(V    , IV  , NV, VG    , NVG ,
     .                        NBLOC, NDDL, IS, ISUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IV(*), NV, NVG, NBLOC, NDDL, IS, ISUM
      my_real
     .        V(*), VG(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, ITAG, NVP, IVP(NDDL), J, JJ, NN, IAD,
     .        ISTAT(MPI_STATUS_SIZE), IERR
      my_real
     .        VV(NDDL), VP(NDDL)
      INTEGER MSGOFF,MSGOFF2,MSGOFF3,MSGOFF4
      DATA MSGOFF/16057/,MSGOFF2/16058/,MSGOFF3/16059/,MSGOFF4/16060/

C
      IF (IS.EQ.1) THEN
C Passage de la decomposition DOMDEC a la process grid PARPACK
         IF (ISPMD.EQ.0) THEN
            DO I=1,NDDL
               VV(I)=ZERO
            ENDDO
C
            DO I=1,NV
               II=IV(I)
               VV(II)=V(I)
            ENDDO
C
            DO I=1,NSPMD-1
               ITAG=MSGOFF 
               CALL MPI_RECV(NVP, 1, MPI_INTEGER, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD, ISTAT, IERR)
               ITAG=MSGOFF2           
               CALL MPI_RECV(IVP, NVP, MPI_INTEGER, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD, ISTAT, IERR)
               ITAG=MSGOFF3             
               CALL MPI_RECV(VP, NVP, REAL, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD, ISTAT, IERR)
               IF (ISUM.EQ.1) THEN
C Pas de sommation des contributions : valable pour des deplacements
                  DO J=1,NVP
                     JJ=IVP(J)
                     VV(JJ)=VP(J)
                  ENDDO
               ELSEIF (ISUM.EQ.2) THEN
C Sommation des contributions : valable pour des forces
                  DO J=1,NVP
                     JJ=IVP(J)
                     VV(JJ)=VV(JJ)+VP(J)
                  ENDDO
               ENDIF
            ENDDO
C
            NN=MIN(NBLOC,NDDL)
            DO I=1,NN
               VG(I)=VV(I)
            ENDDO
C
            DO I=1,NSPMD-1
               IAD=NBLOC*I+1
               NN=MIN(NBLOC, NDDL-IAD+1)
               ITAG=MSGOFF4
               CALL MPI_SEND(VV(IAD), NN, REAL, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD,  IERR)
            ENDDO
         ELSE
            ITAG=MSGOFF
            CALL MPI_SEND(NV, 1, MPI_INTEGER, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD,  IERR)
            ITAG=MSGOFF2
            CALL MPI_SEND(IV, NV, MPI_INTEGER, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD,  IERR)
            ITAG=MSGOFF3
            CALL MPI_SEND(V, NV, REAL, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD,  IERR)
C
            ITAG=MSGOFF4                  
            CALL MPI_RECV(VG, NVG, REAL, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD, ISTAT, IERR)
         ENDIF
      ELSEIF (IS.EQ.2) THEN
C Passage de la process grid PARPACK a la decomposition DOMDEC
         IF (ISPMD.EQ.0) THEN
            NN=MIN(NBLOC,NDDL)
            DO I=1,NN
               VV(I)=VG(I)
            ENDDO
C
            DO I=1,NSPMD-1
               IAD=NBLOC*I+1
               NN=MIN(NBLOC,NDDL-IAD+1)
               ITAG=MSGOFF 
               CALL MPI_RECV(VV(IAD), NN, REAL, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD, ISTAT, IERR)
            ENDDO
C
            DO I=1,NV
               II=IV(I)
               V(I)=VV(II)
            ENDDO
C
            DO I=1,NSPMD-1
               ITAG=MSGOFF2 
               CALL MPI_RECV(NVP, 1, MPI_INTEGER, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD, ISTAT, IERR)
               ITAG=MSGOFF3
               CALL MPI_RECV(IVP, NVP, MPI_INTEGER, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD, ISTAT, IERR)
               DO J=1,NVP
                  JJ=IVP(J)
                  VP(J)=VV(JJ)
               ENDDO
               ITAG=MSGOFF4 
               CALL MPI_SEND(VP, NVP, REAL, IT_SPMD(I+1),
     .                       ITAG, MPI_COMM_WORLD, IERR)
            ENDDO
         ELSE
            ITAG=MSGOFF
            CALL MPI_SEND(VG, NVG, REAL, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD,  IERR)
            ITAG=MSGOFF2 
            CALL MPI_SEND(NV, 1, MPI_INTEGER, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD,  IERR)
            ITAG=MSGOFF3 
            CALL MPI_SEND(IV, NV, MPI_INTEGER, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD, IERR)
C
            ITAG=MSGOFF4 
            CALL MPI_RECV(V, NV, REAL, IT_SPMD(1),
     .                    ITAG, MPI_COMM_WORLD, ISTAT, IERR)
         ENDIF
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_ISR                      source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        DIM_FRKM                      source/mpi/implicit/imp_fri.F 
Chd|        DIM_FRKM1                     source/mpi/implicit/imp_fri.F 
Chd|        INI_FRKC                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_ISR(IAD_S,IAD_R,ITS,ITR,SSIZE ,RSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER SSIZE  ,RSIZE
      INTEGER IAD_S(NSPMD+1),IAD_R(NSPMD+1),ITS(SSIZE),ITR(RSIZE)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER
     .        RBUF(RSIZE), SBUF(SSIZE)
      DATA MSGOFF/16032/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------com. de numero nodes SECONDARYs -----
C au l'appel, RSIZE=IAD_R(NSPMD+1)-1 
C             SSIZE=IAD_S(NSPMD+1)-1 
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_R(I+1)-IAD_R(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_S(I),IAD_S(I+1)-1
          SBUF(L)=ITS(J)
          L = L + 1
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_S(I+1)-IAD_S(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_R(I+1).GT.IAD_R(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_R(I),IAD_R(I+1)-1
          ITR(J) = RBUF(L)
          L = L + 1
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_S(I+1)-IAD_S(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_EXCI                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        INI_FRUD                      source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRK1                     source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCI(ITS,ITR,IAD_S,IAD_R,SIZE ,SSIZE ,RSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER SSIZE  ,RSIZE,SIZE
      INTEGER ITS(SIZE,SSIZE),ITR(SIZE,RSIZE),
     .        IAD_S(NSPMD+1),IAD_R(NSPMD+1)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),K,
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER
     .        RBUF(RSIZE*SIZE), SBUF(SSIZE*SIZE)
      DATA MSGOFF/16033/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------com. de numero nodes SECONDARYs -----
C au l'appel, RSIZE=IAD_R(NSPMD+1)-1 
C             SSIZE=IAD_S(NSPMD+1)-1 
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_R(I+1)-IAD_R(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=0
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_S(I),IAD_S(I+1)-1
          DO K =1,SIZE
           SBUF(L+K)=ITS(K,J)
          ENDDO
          L = L + SIZE
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_S(I+1)-IAD_S(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=0
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_R(I+1).GT.IAD_R(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_R(I),IAD_R(I+1)-1
          DO K =1,SIZE
           ITR(K,J) = RBUF(L+K)
          ENDDO
          L = L + SIZE
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_S(I+1)-IAD_S(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_EXCK                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        SCOM_FRK                      source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRK1                     source/mpi/implicit/imp_fri.F 
Chd|        SCOM_FRUD                     source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCK(KS11,KR11,IAD_S,IAD_R,SIZE ,SSIZE,RSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER SSIZE  ,RSIZE,IAD_S(*),IAD_R(*),SIZE 
      my_real
     .     KS11(SIZE,*),KR11(SIZE,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .      RBUF(SIZE*RSIZE),SBUF(SIZE*SSIZE)
      DATA MSGOFF/16034/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_R(I+1)-IAD_R(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=0
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_S(I),IAD_S(I+1)-1
          DO NOD = 1,SIZE
           SBUF(L+NOD)=KS11(NOD,J)
          ENDDO
          L = L+SIZE
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = (IAD_S(I+1)-IAD_S(I))*SIZE
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=0
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_R(I+1).GT.IAD_R(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_R(I),IAD_R(I+1)-1
          DO NOD = 1,SIZE
           KR11(NOD,J)=RBUF(L+NOD)
          ENDDO
          L = L+SIZE
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_S(I+1)-IAD_S(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_IFRU                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFRU(LX  )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      my_real
     .        LX(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,ID,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(NDDL_SI), SBUF(NDDL_SL)
      DATA MSGOFF/16061/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ com. de D nodes SECONDARYs -----
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SREM(I+1)-IAD_SREM(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SL(I),IAD_SL(I+1)-1
          ID=IDDL_SL(J)
          SBUF(L)  = LX(ID)
          L = L+1
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SL(I+1)-IAD_SL(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SREM(I+1).GT.IAD_SREM(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
          USI(J) = RBUF(L)
          L = L+1
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SL(I+1)-IAD_SL(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_IFRF                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        FR_MATV                       source/mpi/implicit/imp_fri.F 
Chd|        IMP_FRKD                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFRF(F_IMP )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      my_real
     .        F_IMP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,ID,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(NDDL_SL), SBUF(NDDL_SI)
      DATA MSGOFF/16035/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ comm. de F nodes SECONDARYs -----
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SL(I+1)-IAD_SL(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
          SBUF(L)  = FSI(J)
          L = L + 1
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SREM(I+1)-IAD_SREM(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SL(I+1).GT.IAD_SL(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SL(I),IAD_SL(I+1)-1
          ID=IDDL_SL(J)
          F_IMP(ID) = F_IMP(ID) + RBUF(L)
          L = L + 1
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SREM(I+1)-IAD_SREM(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_IFRU_GPU                 source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        FR_MATV_GPU                   source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFRU_GPU(LX,NINDEX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NINDEX(*)
      my_real
     .        LX(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,II,ID,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(NDDL_SI), SBUF(NDDL_SL)
      DATA MSGOFF/16036/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ com. de D nodes SECONDARYs -----
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SREM(I+1)-IAD_SREM(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SL(I),IAD_SL(I+1)-1
          ID=IDDL_SL(J)
          II=NINDEX(ID)
          SBUF(L)  = LX(II)
          L = L+1
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SL(I+1)-IAD_SL(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SREM(I+1).GT.IAD_SREM(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
          USI(J) = RBUF(L)
          L = L+1
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SL(I+1)-IAD_SL(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_IFRF_GPU                 source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        FR_MATV_GPU                   source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFRF_GPU(F_IMP,NINDEX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NINDEX(*)
      my_real
     .        F_IMP(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,II,ID,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      my_real
     .        RBUF(NDDL_SL), SBUF(NDDL_SI)
      DATA MSGOFF/16037/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ comm. de F nodes SECONDARYs -----
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SL(I+1)-IAD_SL(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
          SBUF(L)  = FSI(J)
          L = L + 1
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SREM(I+1)-IAD_SREM(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SL(I+1).GT.IAD_SL(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SL(I),IAD_SL(I+1)-1
          ID=IDDL_SL(J)
          II = NINDEX(ID)
          F_IMP(II) = F_IMP(II) + RBUF(L)
          L = L + 1
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SREM(I+1)-IAD_SREM(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_IFRI                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        MUMPS_SET                     source/implicit/imp_mumps.F   
Chd|        MUMPS_SET2                    source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|        IMP_INTM                      share/modules/imp_intm.F      
Chd|====================================================================
      SUBROUTINE SPMD_IFRI(IG, IL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_INTM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER  IG(*), IL(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,ID,LOC_PROC,IERROR,
     .        SIZ,J,L,STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD),K
      INTEGER RBUF(NDDL_SI), SBUF(NDDL_SL)
      DATA MSGOFF/13038/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C ------ com. de D nodes SECONDARYs -----
C------recive--
        L=1
        DO I=1,NSPMD
         IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SREM(I+1)-IAD_SREM(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
           L = L+SIZ
          ENDIF
         ENDIF
       END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
       L=1
       DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
         DO J=IAD_SL(I),IAD_SL(I+1)-1
           ID=IDDL_SL(J)
            SBUF(L)  = IG(ID)
            L = L+1
         ENDDO
        ENDIF
       ENDDO
C
C   echange messages
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC)THEN
          SIZ = IAD_SL(I+1)-IAD_SL(I)
          IF(SIZ.GT.0)THEN
           MSGTYP = MSGOFF 
           CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
           L = L+SIZ
          ENDIF        
       ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
       L=1
      DO I=1,NSPMD
        IF(I.NE.LOC_PROC.AND.IAD_SREM(I+1).GT.IAD_SREM(I))THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         DO J=IAD_SREM(I),IAD_SREM(I+1)-1
           IL(J) = RBUF(L)
           L = L+1
         ENDDO
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(IAD_SL(I+1)-IAD_SL(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_SEND_VI                  source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_COMPABP                   source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SEND_VI(
     1   NV       ,NSIZ    ,VI       ,NVMAX     ,IOUT     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      INTEGER NV       ,NSIZ    ,NVMAX     ,IOUT
      INTEGER VI(NSIZ,*)       
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ, J , K , L ,IT ,IP ,NN ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      INTEGER
     .        NBUF(NSPMD),RBUF(NSIZ*NVMAX,NSPMD)
      CHARACTER*25  MSG_TYPE(2)
      CHARACTER*25  CSP
      DATA MSGOFF/16039/
      DATA MSGOFF2/16040/
      DATA             
     .            MSG_TYPE
     . / '** WARNING **',
     .   '!! ERROR !!'/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ = 1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(1,I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          NBUF(I)=RBUF(1,I)
        END DO
        NBUF(1) = NV
C
      ELSE
        RBUF(1,1) = NV
        MSGTYP = MSGOFF 
        CALL MPI_SEND(RBUF,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
      END IF
C
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          SIZ = NSIZ*NBUF(I)
          MSGTYP=MSGOFF2 
          CALL MPI_IRECV(RBUF(1,I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
          I = 1
          L = 1
         IF(NSIZ.EQ.1) THEN
          DO J = 1, NBUF(I)
           NN = RBUF(L,I)
           WRITE(CSP,'(A,I2.2)')'INTERFACE TYPE ',NN
           WRITE(IOUT,1100)MSG_TYPE(1),CSP
           L = L + 1
          END DO
         ELSEIF(NSIZ.EQ.3) THEN
          DO J = 1, NBUF(I)
           IP = 1000 + RBUF(L,I)
           IT = RBUF(L+1,I)
           NN = RBUF(L+2,I)
           SELECT CASE(IP)
            CASE (1)
             WRITE(IOUT,1001)MSG_TYPE(IT),NN           
            CASE (2)
             WRITE(IOUT,1002)MSG_TYPE(IT),NN           
            CASE (3)
             WRITE(IOUT,1003)MSG_TYPE(IT),NN           
            CASE (4)
             WRITE(IOUT,1004)MSG_TYPE(IT),NN           
            CASE (5)
             WRITE(IOUT,1005)MSG_TYPE(IT),NN           
            CASE (6)
             WRITE(IOUT,1006)MSG_TYPE(IT),NN           
            CASE (7)
             WRITE(IOUT,1007)MSG_TYPE(IT),NN           
            CASE (8)
             WRITE(IOUT,1008)MSG_TYPE(IT),NN           
            CASE (9)
             WRITE(IOUT,1009)MSG_TYPE(IT),NN           
            CASE (10)
             WRITE(IOUT,1010)MSG_TYPE(IT),NN           
            CASE (11)
             WRITE(IOUT,1011)MSG_TYPE(IT),NN           
            CASE (12)
             WRITE(IOUT,1012)MSG_TYPE(IT),NN           
            CASE (13)
             WRITE(IOUT,1013)MSG_TYPE(IT),NN           
            CASE (14)
             WRITE(IOUT,1014)MSG_TYPE(IT),NN           
            CASE (15)
             WRITE(IOUT,1015)MSG_TYPE(IT),NN           
           END SELECT 
           L = L + 3
          END DO
         END IF
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
C----------write in *.lis-----------
          I = INDEX+1
          L = 1
         IF(NSIZ.EQ.1) THEN
          DO J = 1, NBUF(I)
               NN = RBUF(L,I)
           WRITE(CSP,'(A,I2.2)')'INTERFACE TYPE ',NN
           WRITE(IOUT,1100)MSG_TYPE(1),CSP
           L = L + 1
          END DO
         ELSEIF(NSIZ.EQ.3) THEN
          DO J = 1, NBUF(I)
           IP = RBUF(L,I)
           IT = RBUF(L+1,I)
           NN = RBUF(L+2,I)
           SELECT CASE(IP)
            CASE (1)
             WRITE(IOUT,1001)MSG_TYPE(IT),NN           
            CASE (2)
             WRITE(IOUT,1002)MSG_TYPE(IT),NN           
            CASE (3)
             WRITE(IOUT,1003)MSG_TYPE(IT),NN           
            CASE (4)
             WRITE(IOUT,1004)MSG_TYPE(IT),NN           
            CASE (5)
             WRITE(IOUT,1005)MSG_TYPE(IT),NN           
            CASE (6)
             WRITE(IOUT,1006)MSG_TYPE(IT),NN           
            CASE (7)
             WRITE(IOUT,1007)MSG_TYPE(IT),NN           
            CASE (8)
             WRITE(IOUT,1008)MSG_TYPE(IT),NN           
            CASE (9)
             WRITE(IOUT,1009)MSG_TYPE(IT),NN           
            CASE (10)
             WRITE(IOUT,1010)MSG_TYPE(IT),NN           
            CASE (11)
             WRITE(IOUT,1011)MSG_TYPE(IT),NN           
            CASE (12)
             WRITE(IOUT,1012)MSG_TYPE(IT),NN           
            CASE (13)
             WRITE(IOUT,1013)MSG_TYPE(IT),NN           
            CASE (14)
             WRITE(IOUT,1014)MSG_TYPE(IT),NN           
            CASE (15)
             WRITE(IOUT,1015)MSG_TYPE(IT),NN           
           END SELECT 
           L = L + 3
          END DO
         END IF
        END DO
C
      ELSE
        L = 0
        DO J = 1, NV
         DO K = 1, NSIZ
          L = L + 1
          RBUF(L,1) = VI(K,J)
         END DO
        END DO
        SIZ = NSIZ*NV
        MSGTYP = MSGOFF2 
        CALL MPI_SEND(RBUF,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
      END IF
      RETURN
 1001 FORMAT(A,' NODE USED FOR DIFF. RBODY MAIN=',I8)
 1002 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       'RBODY MAIN AND INTERF. TYPE2 SECONDARY =',I8)
 1003 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       'INTERF. TYPE2 SECONDARY AND INTERF. TYPE2 SECONDARY=',I8)
 1004 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' RBODY MAIN AND RBODY SECONDARY=',I8)
 1005 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' INTERF. TYPE2 SECONDARY AND RBODY SECONDARY=',I8)
 1006 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' RBODY SECONDARY AND RBODY SECONDARY=',I8)
 1007 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' BOUNDARY CONDITIONS AND INTERF. TYPE2 SECONDARY=',I8)
 1008 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' BOUNDARY CONDITIONS AND RBODY SECONDARY=',I8)
 1009 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' IMPOSED DISP. AND INTERF. TYPE2 SECONDARY=',I8)
 1010 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' IMPOSED DISP. AND RBODY SECONDARY=',I8)
 1011 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' IMPOSED DISP. AND BOUNDARY CONDITIONS=',I8)
 1012 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' RWALL CONTACT AND INTERF. TYPE2 SECONDARY=',I8)
 1013 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' RWALL CONTACT AND RBODY SECONDARY=',I8)
 1014 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' RWALL CONTACT AND BOUNDARY CONDITIONS=',I8)
 1015 FORMAT(A,' INCOMPABILITY NODE BETWEEN ',/
     .       ' RWALL CONTACT AND IMPOSED DISP.=',I8)
 1100 FORMAT(A,' IMPLICIT IS INCOMPABLE WITH :',A/)
      END
Chd|====================================================================
Chd|  SPMD_SEND_VR                  source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        IMP_CHECK0                    source/implicit/imp_solv.F    
Chd|        IMP_CHECM0                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SEND_VR(
     1   NV       ,NSIZ    ,VR       ,NVMAX     ,IOUT     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      INTEGER NV       ,NSIZ    ,NVMAX     ,IOUT
      my_real
     .        VR(NSIZ,*)       
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ, J , K , L ,IT ,IP ,NN ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      INTEGER
     .        NBUF(NSPMD),NR(NSPMD)
      my_real
     .       RBUF(NSIZ*NVMAX,NSPMD)  ,S
      CHARACTER  DIR(3)
      DATA DIR/'X','Y','Z'/           
      DATA MSGOFF/16041/
      DATA MSGOFF2/16042/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ = 1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(NBUF(I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          NR(I)=NBUF(I)
        END DO
        NR(1) = NV
C
      ELSE
        NBUF(1) = NV
        MSGTYP = MSGOFF 
        CALL MPI_SEND(NBUF,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
      END IF
C
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          SIZ = NSIZ*NR(I)
          MSGTYP=MSGOFF2
          CALL MPI_IRECV(RBUF(1,I),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        I = 1
        L = 1
        DO J = 1, NR(I)
          NN = INT(VR(L,I))
          IT = INT(VR(L+1,I))
          S = VR(L+2,I)
             IF (IT.LE.3) THEN
              WRITE(IOUT,1001)NN,DIR(IT),S
             ELSE 
              WRITE(IOUT,1002)NN,DIR(IT-3),S
             ENDIF
          L = L + 3
        END DO
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
C----------write in *.lis-----------
          I = INDEX+1
         L = 1
         DO J = 1, NR(I)
          NN = INT(RBUF(L,I))
          IT = INT(RBUF(L+1,I))
          S = RBUF(L+2,I)
             IF (IT.LE.3) THEN
              WRITE(IOUT,1001)NN,DIR(IT),S
             ELSE 
              WRITE(IOUT,1002)NN,DIR(IT-3),S
             ENDIF
          L = L + 3
         END DO
        END DO
C
      ELSE
        L = 0
        DO J = 1, NV
         DO K = 1, NSIZ
          L = L + 1
          RBUF(L,1) = VR(K,J)
         END DO
        END DO
        MSGTYP = MSGOFF2 
        SIZ = NSIZ*NV
        CALL MPI_SEND(RBUF,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
      END IF
      RETURN
 1001 FORMAT(' NODE NUM. =',I10,5X,'TRA_DIR = ',1A,5X,'VAL.= ',G14.7)
 1002 FORMAT(' NODE NUM. =',I10,5X,'ROT_DIR = ',1A,5X,'VAL.= ',G14.7)
      END

Chd|====================================================================
Chd|  SPMD_NDDLIG                   source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        GETNDDLI_G                    source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_NDDLIG(NDDL    ,NDDLFR   ,NDDLG    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      INTEGER NDDL    ,NDDLFR   ,NDDLG
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      INTEGER
     .        RBUF(NSPMD),SI
      DATA MSGOFF/16043/,MSGOFF2/16044/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ=1
      RBUF(1) = NDDL
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(I),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          RBUF(1) = RBUF(1) + RBUF(I)
        END DO
C
        NDDLG = RBUF(1)-NDDLFR
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(NDDLG,SIZ,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
C
      ELSE
        MSGTYP = MSGOFF 
        CALL MPI_SEND(RBUF,SIZ,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        MSGTYP = MSGOFF2 
        CALL MPI_RECV(NDDLG,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
      END IF
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_MAX_II                   source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|        SMS_RBE3_NODXI                source/ams/sms_rbe3.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MAX_II(NMAX,IAD_ELEM,TSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER NMAX,IAD_ELEM(*),TSIZE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,NOD,LOC_PROC,IERROR,
     .        STATUS(MPI_STATUS_SIZE),SIZ,
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        I,J,K,L
      INTEGER
     .        RBUF(TSIZE), SBUF(TSIZE)
      DATA MSGOFF/16045/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      DO I=1,NSPMD
        SIZ = IAD_ELEM(I+1)-IAD_ELEM(I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(I)
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      DO I=1,NSPMD
       DO J=IAD_ELEM(I),IAD_ELEM(I+1)-1
        SBUF(J) = NMAX
       ENDDO
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      DO I=1,NSPMD
        SIZ = IAD_ELEM(I+1)-IAD_ELEM(I)
        IF(SIZ.GT.0)THEN
          L = IAD_ELEM(I)
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      DO I=1,NSPMD
       SIZ = IAD_ELEM(I+1)-IAD_ELEM(I)
       IF(SIZ.GT.0)THEN
        CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
        DO J=IAD_ELEM(I),IAD_ELEM(I+1)-1
         NMAX = MAX(NMAX,RBUF(J))
        ENDDO
       ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF((IAD_ELEM(I+1)-IAD_ELEM(I)).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_MAX_F                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        PR_SOLNFO                     source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_MAX_F(F,ITAB,K)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
      INTEGER ITAB, K
      my_real
     .        F
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        MSGTYP2, MSGTYP3, MSGOFF3, MSGOFF4, MSGOFF5, MSGOFF6,
     .        INDEX, SIZ,SIZ2,SIZ3,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD),
     .        IBUF1(NSPMD), IBUF2(NSPMD)
      my_real
     .        RBUF(NSPMD),SI
      DATA MSGOFF/16046/,MSGOFF2/16047/
      DATA MSGOFF3/16048/,MSGOFF4/16049/
      DATA MSGOFF5/16050/,MSGOFF6/16051/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      SIZ=1
      SIZ2=1
      SIZ3=1
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          MSGTYP2=MSGOFF3 
          MSGTYP3=MSGOFF5 
          CALL MPI_IRECV(RBUF(I),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
          CALL MPI_IRECV(IBUF1(I),SIZ2,MPI_INTEGER,IT_SPMD(I),MSGTYP2,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
          CALL MPI_IRECV(IBUF2(I),SIZ3,MPI_INTEGER,IT_SPMD(I),MSGTYP3,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          IF (ABS(RBUF(I)) > ABS(F)) THEN
            F = RBUF(I)
            ITAB = IBUF1(I)
            K = IBUF2(I)
          ENDIF
        END DO
C
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          MSGTYP2=MSGOFF4 
          MSGTYP3=MSGOFF6 
          CALL MPI_SEND(F,SIZ,REAL,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
          CALL MPI_SEND(ITAB,SIZ2,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP2,MPI_COMM_WORLD,IERROR)
          CALL MPI_SEND(K,SIZ3,MPI_INTEGER,IT_SPMD(I),
     .                  MSGTYP3,MPI_COMM_WORLD,IERROR)
        END DO
      ELSE
        MSGTYP = MSGOFF 
        MSGTYP2 = MSGOFF3 
        MSGTYP3 = MSGOFF5 
        CALL MPI_SEND(F,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        CALL MPI_SEND(ITAB,SIZ2,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP2,MPI_COMM_WORLD,IERROR)
        CALL MPI_SEND(K,SIZ3,MPI_INTEGER,IT_SPMD(1),
     .                MSGTYP3,MPI_COMM_WORLD,IERROR)
        MSGTYP = MSGOFF2 
        MSGTYP2 = MSGOFF4 
        MSGTYP3 = MSGOFF6 
        CALL MPI_RECV(F,SIZ,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
        CALL MPI_RECV(ITAB,SIZ2,MPI_INTEGER,IT_SPMD(1),MSGTYP2,
     .                MPI_COMM_WORLD,STATUS,IERROR)
        CALL MPI_RECV(K,SIZ3,MPI_INTEGER,IT_SPMD(1),MSGTYP3,
     .                MPI_COMM_WORLD,STATUS,IERROR)
     
      END IF
C
      RETURN
      END

Chd|====================================================================
Chd|  SPMD_MAX_IV                   source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        UPD_FR_K                      source/mpi/implicit/imp_fri.F 
Chd|-- calls ---------------
Chd|        IMP_FRK                       share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_MAX_IV(IV   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_FRK
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER
     .        IV(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER MSGOFF,MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,ND,ID,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER
     .        RBUF(NDDLFR), SBUF(NDDLFR)
      DATA MSGOFF/16052/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      IF (NDDLFR.LE.0) RETURN
C
      LOC_PROC = ISPMD + 1
C
      L=1
      DO I=1,NSPMD
        SIZ = ND_FR(I)
        IF(SIZ.GT.0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(L),SIZ  ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L+ ND_FR(I)
        ENDIF
      END DO
C
C preparation envoi  a proc I
C--------------------------------------------------------------------
      L = 0
      DO I=1,NSPMD
        IF(ND_FR(I).GT.0)THEN
          DO J=1,ND_FR(I)
           ID = J + L
           SBUF(ID) = IV(ID)
          ENDDO
          L = L +ND_FR(I) 
        ENDIF
      ENDDO
C
C   echange messages
C--------------------------------------------------------------------
      L = 1
      DO I=1,NSPMD
        SIZ = ND_FR(I)
        IF(SIZ.GT.0)THEN
          MSGTYP = MSGOFF 
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
         L = L +ND_FR(I) 
        ENDIF        
      ENDDO
C
C assemblage
C--------------------------------------------------------------------
      L = 0
      DO I=1,NSPMD
        IF(ND_FR(I).GT.0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          DO J=1,ND_FR(I)
           ID = J + L
           IV(ID) = MAX(IV(ID) ,RBUF(ID) )
          ENDDO
          L = L +ND_FR(I) 
        ENDIF
      ENDDO
C
Cel wait terminaison isend
C--------------------------------------------------------------------
      DO I = 1, NSPMD
        IF(ND_FR(I).GT.0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_INT_ALLREDUCE_MAX        source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        PRINT_STIFF_MAT               source/implicit/imp_mumps.F   
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_INT_ALLREDUCE_MAX(SENDBUF,RECVBUF,COUNT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      INTEGER SENDBUF(*),RECVBUF(*),COUNT
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C MPI_ALLREDUCE with MAX function applied to table of integers
      CALL MPI_ALLREDUCE(SENDBUF,RECVBUF,COUNT,
     .                     MPI_INTEGER,MPI_MAX,
     .                     MPI_COMM_WORLD,IERROR)
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_E_REF                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_E_REF(DMIN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_SPBRM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
      my_real
     .        DMIN
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,IBUF(2),L,J,K,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      my_real
     .        RBUF(32+10,NSPMD),SI,SBUF(31+10)
      DATA MSGOFF/16053/
      DATA MSGOFF2/16054/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
       SIZ = 32+10
       RBUF(1,1) = DMIN
       RBUF(2,1) = N_SEG
       RBUF(3:8,1) = IKCE(1:6)
       L = 8
       DO I = 1,4    
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = X_REF(K,I)
         END DO
       END DO
       DO I = 1,4    
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = D_REF(K,I)
         END DO
       END DO
C
        DO I = 1,9
          L = L + 1
          RBUF(L,1) = RLSKEW(I)
        END DO 
        L = L + 1
        RBUF(L,1) = LSKEW_G
C        
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(1,I),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
          IF (RBUF(1,1) > RBUF(1,I)) THEN
           RBUF(1,1) = RBUF(1,I)
           RBUF(2:8,1) = RBUF(2:8,I)
           L = 8
           DO J = 1,4    
             DO K = 1,3
              L = L + 1
              RBUF(L,1) = RBUF(L,I)
             END DO
           END DO
           DO J = 1,4    
             DO K = 1,3
              L = L + 1
              RBUF(L,1) = RBUF(L,I)
             END DO
           END DO
           DO K = 1,10
            L = L + 1
            RBUF(L,1) = RBUF(L,I)
           END DO 
          END IF
        END DO
C
        L = 31+10
        SBUF(1:L)=RBUF(2:L+1,1)
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(SBUF,L,REAL,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
C
      ELSE
        MSGTYP = MSGOFF 
        L = 31+10
        CALL MPI_SEND(RBUF,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        MSGTYP = MSGOFF2 
        CALL MPI_RECV(SBUF,L,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
         RBUF(2:L+1,1) = SBUF(1:L)
      END IF
      N_SEG = INT(RBUF(2,1)) 
      IKCE(1:6) = INT(RBUF(3:8,1)) 
       L = 8
       DO I = 1,4    
         DO K = 1,3
          L = L + 1
          X_REF(K,I) = RBUF(L,1)
         END DO
       END DO
       DO I = 1,4    
         DO K = 1,3
          L = L + 1
          D_REF(K,I) = RBUF(L,1)
         END DO
       END DO
        DO I = 1,9
          L = L + 1
          RLSKEW(I) = RBUF(L,1)
        END DO 
        L = L + 1
        LSKEW_G = INT(RBUF(L,1)) 
C
      RETURN
      END
Chd|====================================================================
Chd|  SPMD_N_REF                    source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|        SPB_RM_RIG                    source/implicit/imp_solv.F    
Chd|-- calls ---------------
Chd|        IMP_SPBRM                     share/modules/impbufdef_mod.F 
Chd|====================================================================
      SUBROUTINE SPMD_N_REF
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE IMP_SPBRM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef  MPI
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
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, MSGOFF, MSGOFF2, MSGTYP, IERROR, LOC_PROC,
     .        INDEX, SIZ,IBUF(2),L,J,K,
     .        STATUS(MPI_STATUS_SIZE), REQ_R(NSPMD)
      my_real
     .        RBUF(27+10,NSPMD),SI,SBUF(27+10)
      DATA MSGOFF/16053/
      DATA MSGOFF2/16054/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
       SIZ = 27+10
C------- 3xRBUF(1:7,1) :N1,X1,D1; RBUF(22,1) :ILSKEW; RBUF((23:37,1) :RLSKEW,IKCE    
       L = 1
       I = 1
       RBUF(L,1) = E_REF(1)
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = X_REF(K,I)
         END DO
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = D_REF(K,I)
         END DO
       I = 2
         L = L + 1
         RBUF(L,1) = E_REF(2)
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = X_REF(K,I)
         END DO
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = D_REF(K,I)
         END DO
       I = 3
         L = L + 1
         RBUF(L,1) = E_REF(3)
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = X_REF(K,I)
         END DO
         DO K = 1,3
          L = L + 1
          RBUF(L,1) = D_REF(K,I)
         END DO
C
         L = L + 1
         RBUF(L,1) = LSKEW_G
        DO I = 1,9
          L = L + 1
          RBUF(L,1) = RLSKEW(I)
        END DO 
        DO I = 1,6
          L = L + 1
          RBUF(L,1) = IKCE(I)
        END DO 
C        
      IF(ISPMD.EQ.0) THEN
        DO I = 2, NSPMD
          MSGTYP=MSGOFF 
          CALL MPI_IRECV(RBUF(1,I),SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,REQ_R(I-1),IERROR)
        END DO
C
        DO N = 1, NSPMD-1
          CALL MPI_WAITANY(NSPMD-1,REQ_R,INDEX,STATUS,IERROR)
          I = INDEX+1
         L = 1 ! K = 1
          IF (RBUF(L,1)==ZERO .AND. RBUF(L,I)>ZERO) THEN
           RBUF(L,1) = RBUF(L,I)
c           E_REF(1) = INT(RBUF(L,1))
           RBUF(L+1:L+6,1) = RBUF(L+1:L+6,I)
          END IF
         L = 8 ! K = 2
          IF (RBUF(L,1)==ZERO .AND. RBUF(L,I)>ZERO) THEN
           RBUF(L,1) = RBUF(L,I)
c           E_REF(2) = INT(RBUF(L,1))
           RBUF(L+1:L+6,1) = RBUF(L+1:L+6,I)
          END IF
         L = 15 ! K = 3
          IF (RBUF(L,1)==ZERO .AND. RBUF(L,I)>ZERO) THEN
           RBUF(L,1) = RBUF(L,I)
c           E_REF(3) = INT(RBUF(L,1))
           RBUF(L+1:L+6,1) = RBUF(L+1:L+6,I)
          END IF
          L = 22
          IF (RBUF(L,1)==ZERO .AND. RBUF(L,I)>ZERO) THEN
           RBUF(L,1) = RBUF(L,I)
           RBUF(L+1:L+9,1) = RBUF(L+1:L+9,I)
          END IF
          DO K=L+10,L+15
            IF (RBUF(K,1)==ZERO .AND. RBUF(K,I)>ZERO) THEN
             RBUF(K,1) = RBUF(K,I)
            END IF
          END DO
        END DO
C
        L = 27+10
        SBUF(1:L)=RBUF(1:L,1)
        DO I = 2, NSPMD
          MSGTYP=MSGOFF2 
          CALL MPI_SEND(SBUF,L,REAL,IT_SPMD(I),
     .                  MSGTYP,MPI_COMM_WORLD,IERROR)
        END DO
C
      ELSE
        MSGTYP = MSGOFF 
        L = 27+10
        CALL MPI_SEND(RBUF,SIZ,REAL,IT_SPMD(1),
     .                MSGTYP,MPI_COMM_WORLD,IERROR)
        MSGTYP = MSGOFF2 
        CALL MPI_RECV(SBUF,L,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,STATUS,IERROR)
         RBUF(1:L,1) = SBUF(1:L)
      END IF
      N_SEG = 3 
       L = 1
       I = 1
c       RBUF(L,1) = E_REF(1)
         DO K = 1,3
          L = L + 1
          X_REF(K,I) = RBUF(L,1)
         END DO
         DO K = 1,3
          L = L + 1
          D_REF(K,I) = RBUF(L,1)
         END DO
       I = 2
         L = L + 1
         DO K = 1,3
          L = L + 1
          X_REF(K,I) = RBUF(L,1)
         END DO
         DO K = 1,3
          L = L + 1
          D_REF(K,I) = RBUF(L,1)
         END DO
c         RBUF(L,1) = E_REF(2)
       I = 3
         L = L + 1
         DO K = 1,3
          L = L + 1
          X_REF(K,I) = RBUF(L,1)
         END DO
         DO K = 1,3
          L = L + 1
          D_REF(K,I) = RBUF(L,1)
         END DO
C
         L = L + 1
         LSKEW_G = INT(RBUF(L,1))          
        DO I = 1,6
          L = L + 1
          IKCE(I) = INT(RBUF(L,1)) 
        END DO 
        DO I = 1,9
          L = L + 1
          RLSKEW(I) = RBUF(L,1) 
        END DO 
C
      RETURN
      END


#else
C
Chd|====================================================================
Chd|  BIDON_IMP                     source/mpi/implicit/imp_spmd.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BIDON_IMP
C-----------------------------------------------
#include "implicit_f.inc"
#ifdef MUMPS5
#include      "dmumps_struc.h"
#endif
C-----------------------------------------------
      INTEGER
     .        IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     .        IDUM6, IDUM7, IDUM8, IDUM9, IDUM0,
     .        IDUM11,IDUM12,IDUM13,IDUM14,IDUM15,
     .        IDUM16
      my_real
     .        RDUM1, RDUM2, RDUM3, RDUM4, RDUM5,
     .        RDUM6, RDUM7, RDUM8, RDUM9, RDUM0,
     .        RDUMA, RDUMB
#ifdef MUMPS5
      TYPE(DMUMPS_STRUC) MUMPS_PAR
#endif
C
      ENTRY SPMD_SUM_S(RDUM1)
      ENTRY SPMD_SUM_S2(RDUM1,IDUM1)
      ENTRY SPMD_MAX_S(RDUM1)
      ENTRY SPMD_MIN_S(RDUM1)
      ENTRY SPMD_MAX_I(IDUM1)
      ENTRY SPMD_INF_G(
     .        IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     .        IDUM6, IDUM7, IDUM8, IDUM9, IDUM0)
      ENTRY SPMD_SUMF_V(RDUM1)
      ENTRY SPMD_SUMFC_V(RDUM1,RDUM2,IDUM1)
      ENTRY SPMD_SUMF_K(RDUM1, RDUM2)
      ENTRY SPMD_INIS(IDUM1,IDUM2)
      ENTRY SPMD_INISL(IDUM1,IDUM2)
      ENTRY SPMD_IFC1(IDUM1, IDUM2,RDUM1)
      ENTRY SPMD_IFCD(RDUM1,IDUM1, IDUM2)
      ENTRY SPMD_IFCF(RDUM1,IDUM1, IDUM2)
      ENTRY SPMD_NROW(IDUM1, IDUM2, IDUM3, IDUM4)
      ENTRY SPMD_ICOL(
     .        IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     .        IDUM6, IDUM7, IDUM8, IDUM9, IDUM0)
      ENTRY SPMD_I2D(IDUM1, IDUM2, IDUM3, IDUM4)
      ENTRY SPMD_SUMF_A(RDUM1,RDUM2,IDUM1, IDUM2, IDUM3, IDUM4)
      ENTRY SPMD_NDOF(IDUM1, IDUM2, IDUM3, IDUM4)
      ENTRY SPMD_CDDL(IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,
     .                IDUM6, IDUM7, IDUM8, IDUM9)
      ENTRY SPMD_VCHGRID(RDUM1, IDUM1, IDUM2, RDUM2, IDUM3,
     .                   IDUM4, IDUM5, IDUM6,IDUM7)
      ENTRY SPMD_ISR(IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,IDUM6)
      ENTRY SPMD_EXCI(IDUM1, IDUM2, IDUM3, IDUM4, IDUM5,IDUM6,IDUM7)
      ENTRY SPMD_EXCK(RDUM1,RDUM2,IDUM1, IDUM2,IDUM3,IDUM4, IDUM5)
      ENTRY SPMD_IFRU(RDUM1)
      ENTRY SPMD_IFRF(RDUM1)
      ENTRY SPMD_IFRU_GPU(RDUM1,IDUM1)
      ENTRY SPMD_IFRF_GPU(RDUM1,IDUM1)
      ENTRY SPMD_IFRI(IDUM1, IDUM2)
      ENTRY SPMD_SEND_VI(IDUM1, IDUM2, IDUM3, IDUM4, IDUM5)
      ENTRY SPMD_SEND_VR(IDUM1, IDUM2, RDUM3, IDUM4, IDUM5)
      ENTRY SPMD_NDDLIG(IDUM1, IDUM2, IDUM3 )
      ENTRY SPMD_MAX_II(IDUM1, IDUM2, IDUM3)
      ENTRY SPMD_MIN_I(IDUM1)
      ENTRY SPMD_MAX_IV(IDUM1)
      ENTRY SPMD_MAX_F(RDUM1, IDUM1, IDUM2)
      ENTRY SPMD_INT_ALLREDUCE_MAX(IDUM1, IDUM2, IDUM3)
      ENTRY SPMD_E_REF(RDUM1)
      ENTRY SPMD_N_REF

C     ENTRY MPI_BCAST(IDUM1, IDUM2, IDUM3, IDUM4, IDUM5, IDUM6)
C     ENTRY MPI_ALLREDUCE
C     ENTRY MPI_IRECV
C     ENTRY MPI_ISEND
C     ENTRY MPI_WAITANY
C     ENTRY MPI_WAITALL
C     ENTRY MPI_WAIT
C     ENTRY MPI_REDUCE

      END
#endif
C
