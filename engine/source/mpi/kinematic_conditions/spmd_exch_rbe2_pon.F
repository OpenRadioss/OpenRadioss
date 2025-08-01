Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    spmd_exch_rbe2_pon    ../engine/source/mpi/kinematic_conditions/spmd_exch_rbe2_pon.F
!||--- called by ------------------------------------------------------
!||    rbe2t1                ../engine/source/constraints/general/rbe2/rbe2f.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_EXCH_RBE2_PON(
     1   A     ,AR     ,STIFN   ,STIFR  ,IAD_M   ,
     2   FR_M  ,LCOMM  ,ISIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOMM, ISIZE, FR_M(*), IAD_M(*)
      DOUBLE PRECISION
     .        A(3,6,*), AR(3,6,*), STIFN(6,*), STIFR(6,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,K,
     .        STATUS(MPI_STATUS_SIZE),ISIZE6,
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD),IROT
      DATA MSGOFF/211/
      DOUBLE PRECISION
     .        SBUF(LCOMM*ISIZE*6), RBUF(LCOMM*ISIZE*6)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      ISIZE6=ISIZE*6
      IROT = 0
      IF (ISIZE>4) IROT=1
C
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        LEN = IAD_M(I+1)-IAD_M(I)
        IF(LEN>0) THEN
          SIZ = LEN*ISIZE6
          L=L+1
          INDEXI(L)=I
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(IDEB),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      SPMD_COMM_WORLD,REQ_R(L),IERROR)
          IDEB = IDEB + SIZ
        ENDIF
      ENDDO
      NBINDEX = L
C
      IDEB = 1
      DO L = 1, NBINDEX
        I = INDEXI(L)
        LEN = IAD_M(I+1) - IAD_M(I)
        IAD = IAD_M(I)-1
        IF (IROT==0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            DO K = 1, 6
             SBUF(IDEB)   = A(1,K,NOD)
             SBUF(IDEB+1) = A(2,K,NOD)
             SBUF(IDEB+2) = A(3,K,NOD)
             SBUF(IDEB+3) = STIFN(K,NOD)
             IDEB = IDEB + ISIZE
            ENDDO
          ENDDO
        ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            DO K = 1, 6
             SBUF(IDEB)   = A(1,K,NOD)
             SBUF(IDEB+1) = A(2,K,NOD)
             SBUF(IDEB+2) = A(3,K,NOD)
             SBUF(IDEB+3) = AR(1,K,NOD)
             SBUF(IDEB+4) = AR(2,K,NOD)
             SBUF(IDEB+5) = AR(3,K,NOD)
             SBUF(IDEB+6) = STIFN(K,NOD)
             SBUF(IDEB+7) = STIFR(K,NOD)
             IDEB = IDEB + ISIZE
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        LEN = IAD_M(I+1)-IAD_M(I)
        SIZ = LEN*ISIZE6
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G    SPMD_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_R,INDEX,STATUS,IERROR)
        I = INDEXI(INDEX)
        IDEB = 1+(IAD_M(I)-1)*ISIZE6
        LEN = IAD_M(I+1)-IAD_M(I)
        IAD = IAD_M(I)-1
        IF (IROT==0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            DO K = 1, 6
             A(1,K,NOD)   = A(1,K,NOD) + RBUF(IDEB)
             A(2,K,NOD)   = A(2,K,NOD) + RBUF(IDEB+1)
             A(3,K,NOD)   = A(3,K,NOD) + RBUF(IDEB+2)
             STIFN(K,NOD) = STIFN(K,NOD)+RBUF(IDEB+3)
             IDEB = IDEB + ISIZE
            ENDDO
          ENDDO
        ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            DO K = 1, 6
             A(1,K,NOD)   = A(1,K,NOD) + RBUF(IDEB)
             A(2,K,NOD)   = A(2,K,NOD) + RBUF(IDEB+1)
             A(3,K,NOD)   = A(3,K,NOD) + RBUF(IDEB+2)
             AR(1,K,NOD)  = AR(1,K,NOD)+ RBUF(IDEB+3)
             AR(2,K,NOD)  = AR(2,K,NOD)+ RBUF(IDEB+4)
             AR(3,K,NOD)  = AR(3,K,NOD)+ RBUF(IDEB+5)
             STIFN(K,NOD) = STIFN(K,NOD)+RBUF(IDEB+6)
             STIFR(K,NOD) = STIFR(K,NOD)+RBUF(IDEB+7)
             IDEB = IDEB + ISIZE
            ENDDO
          END DO
        END IF
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_S,INDEX,STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END
