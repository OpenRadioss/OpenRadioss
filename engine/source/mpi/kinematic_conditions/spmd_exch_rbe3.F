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
Chd|  SPMD_EXCH_RBE3                source/mpi/kinematic_conditions/spmd_exch_rbe3.F
Chd|-- called by -----------
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_RBE3(
     1   A    ,AR     ,MS    ,IN     ,STIFN,
     2   STIFR,FR_M   ,IAD_M ,LCOMM  ,ISIZE,
     3   IROT  )
C realise le cumul des acc et masses aux noeuds main d'Rbe3
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
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOMM, ISIZE, FR_M(*), IAD_M(*),IROT
      my_real
     .        A(3,*), AR(3,*), MS(*), IN(*),
     .        STIFN(*), STIFR(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD)
      DATA MSGOFF/207/
      my_real
     .        SBUF(LCOMM*ISIZE), RBUF(LCOMM*ISIZE)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        LEN = IAD_M(I+1)-IAD_M(I)
        IF(LEN.GT.0) THEN
          SIZ = LEN*ISIZE
          L=L+1
          INDEXI(L)=I
          MSGTYP = MSGOFF 
          CALL MPI_IRECV(
     S      RBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(L),IERROR)
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
        IF (IROT.EQ.0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)
            SBUF(IDEB+1) = A(2,NOD)
            SBUF(IDEB+2) = A(3,NOD)
            SBUF(IDEB+3) = MS(NOD)
            SBUF(IDEB+4) = STIFN(NOD)
            IDEB = IDEB + ISIZE
          ENDDO
        ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)
            SBUF(IDEB+1) = A(2,NOD)
            SBUF(IDEB+2) = A(3,NOD)
            SBUF(IDEB+3) = AR(1,NOD)
            SBUF(IDEB+4) = AR(2,NOD)
            SBUF(IDEB+5) = AR(3,NOD)
            SBUF(IDEB+6) = MS(NOD)
            SBUF(IDEB+7) = IN(NOD)
            SBUF(IDEB+8) = STIFN(NOD)
            SBUF(IDEB+9) = STIFR(NOD)
            IDEB = IDEB + ISIZE
          ENDDO
        ENDIF
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        LEN = IAD_M(I+1)-IAD_M(I)
        SIZ = LEN*ISIZE
        MSGTYP = MSGOFF 
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_R,INDEX,STATUS,IERROR)
        I = INDEXI(INDEX)
        IDEB = 1+(IAD_M(I)-1)*ISIZE
        LEN = IAD_M(I+1)-IAD_M(I)
        IAD = IAD_M(I)-1
        IF (IROT.EQ.0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+3)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+4)
            IDEB = IDEB + ISIZE
          ENDDO
        ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            AR(1,NOD)  = AR(1,NOD)+ RBUF(IDEB+3)
            AR(2,NOD)  = AR(2,NOD)+ RBUF(IDEB+4)
            AR(3,NOD)  = AR(3,NOD)+ RBUF(IDEB+5)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+6)
            IN(NOD)    = IN(NOD)  + RBUF(IDEB+7)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+8)
            STIFR(NOD) = STIFR(NOD)+RBUF(IDEB+9)
            IDEB = IDEB + ISIZE
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
