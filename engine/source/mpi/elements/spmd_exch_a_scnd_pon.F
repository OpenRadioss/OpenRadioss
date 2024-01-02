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
Chd|  SPMD_EXCH_A_SCND_PON          source/mpi/elements/spmd_exch_a_scnd_pon.F
Chd|-- called by -----------
Chd|        S10CNDAMP                     source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDF1                      source/elements/solid/solide10/s10cndf.F
Chd|        S10CNDF2                      source/elements/solid/solide10/s10cndf.F
Chd|        S10CNIDAMP                    source/elements/solid/solide10/s10cndf.F
Chd|        S10CNISTAT                    source/elements/solid/solide10/s10cndf.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_SCND_PON(
     1   FR_CDNM,IAD_CDNM,ADDCNCDN,PROCNCDN,FR_NBCCCDN,
     2   ISIZE,LENR   ,LENS   ,FSKYCDN)
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
      INTEGER IAD_CDNM(*),FR_CDNM(*),FR_NBCCCDN(2,*),
     .        ADDCNCDN(*), PROCNCDN(*),ISIZE ,LENR ,LENS
      my_real
     .        FSKYCDN(ISIZE,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR, INDEXI, NISKYF, N, IDEB,
     .        SIZ, J, L, CC, NBIRECV, NBISEND, II, MSGOFF,
     .        IAD_RECV(NSPMD+1), INDEXR(NSPMD),INDEXS(NSPMD),
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),K
      my_real
     .        RBUF(LENR*ISIZE),SBUF(LENS*ISIZE)
      DATA MSGOFF/1718/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      NBIRECV = 0
      NBISEND = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        IF(FR_NBCCCDN(2,I)>0) THEN
          SIZ = ISIZE*FR_NBCCCDN(2,I)
          MSGTYP = MSGOFF
          NBIRECV = NBIRECV + 1
          INDEXR(NBIRECV) = I
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
        IF(FR_NBCCCDN(1,I)>0) THEN
          NBISEND = NBISEND + 1
          INDEXS(NBISEND) = I
        ENDIF
      ENDDO
C
C preparation envoi
C
      L = 1
      DO II=1, NBISEND
        I = INDEXS(II)
           DO J=IAD_CDNM(I),IAD_CDNM(I+1)-1
            NOD = FR_CDNM(J)
            DO CC = ADDCNCDN(NOD),ADDCNCDN(NOD+1)-1
              IF(PROCNCDN(CC)==LOC_PROC) THEN
               DO K = 1, ISIZE
                SBUF(L+K-1)   = FSKYCDN(K,CC)
               END DO
                L = L + ISIZE
              ENDIF
            ENDDO
           END DO
      ENDDO
C
C   echange messages
C
      L = 1
      DO II=1,NBISEND
        I = INDEXS(II)
        SIZ = ISIZE*FR_NBCCCDN(1,I) 
        MSGTYP = MSGOFF 
        CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(II),IERROR)
        L = L + SIZ
      ENDDO
C
C decompactage
C
      DO II=1,NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEXI,STATUS,IERROR)
        I = INDEXR(INDEXI)
        L = IAD_RECV(I)
           DO J=IAD_CDNM(I),IAD_CDNM(I+1)-1
            NOD = FR_CDNM(J)
            DO CC = ADDCNCDN(NOD), ADDCNCDN(NOD+1)-1
              IF(PROCNCDN(CC)==I) THEN
               DO K = 1, ISIZE
                FSKYCDN(K,CC) = RBUF(L+K-1)
               END DO 
                L = L + ISIZE
              ENDIF
            ENDDO
           END DO
      END DO
C
C   wait terminaison isend
C
      DO L=1,NBISEND
        CALL MPI_WAITANY(NBISEND,REQ_S,INDEXI,STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END SUBROUTINE


