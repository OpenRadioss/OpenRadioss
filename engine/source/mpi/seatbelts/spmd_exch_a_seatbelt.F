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
Chd|  SPMD_EXCH_A_SEATBELT          source/mpi/seatbelts/spmd_exch_a_seatbelt.F
Chd|-- called by -----------
Chd|        KINE_SEATBELT_FORCE           source/tools/seatbelts/kine_seatbelt_force.F
Chd|-- calls ---------------
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_SEATBELT(A,STIFN,BUF_EXCH)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SEATBELT_MOD
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real ,INTENT(IN) :: BUF_EXCH(N_ANCHOR_REMOTE_SEND,4)
      my_real ,INTENT(INOUT) :: A(3,NUMNOD),STIFN(NUMNOD)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,K,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),OFFSET  
      my_real
     .        RBUF(4*N_ANCHOR_REMOTE),SBUF(4*N_ANCHOR_REMOTE_SEND)  
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IF (N_ANCHOR_REMOTE > 0) THEN
        L = 1
        IAD_RECV(1) = 1      
        DO I=1,NSPMD
          SIZ = 4*(ANCHOR_REMOTE%ADD_PROC(I+1)-ANCHOR_REMOTE%ADD_PROC(I))
          IF(SIZ/=0)THEN
            MSGTYP = 10000 + NSPMD*(I-1) + LOC_PROC
            CALL MPI_IRECV(
     S           RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G           MPI_COMM_WORLD,REQ_R(I),IERROR)
            L = L + SIZ
          ENDIF
          IAD_RECV(I+1) = L
        END DO
      ENDIF
C
      IF (N_ANCHOR_REMOTE_SEND > 0) THEN
        L = 1
        IAD_SEND(1) = 1  
        DO I=1,NSPMD
C preparation envoi  partie fixe (elem) a proc I
#include      "vectorize.inc"
          DO J=ANCHOR_REMOTE_SEND%ADD_PROC(I),ANCHOR_REMOTE_SEND%ADD_PROC(I+1)-1
            NOD = ANCHOR_REMOTE_SEND%NODE(J)
            SBUF(L  ) =  BUF_EXCH(NOD,1)
            SBUF(L+1) =  BUF_EXCH(NOD,2)
            SBUF(L+2) =  BUF_EXCH(NOD,3)
            SBUF(L+3) =  BUF_EXCH(NOD,4)
            L = L + 4
          END DO
          IAD_SEND(I+1) = L
        ENDDO
C
C   echange messages
C
        DO I=1,NSPMD
C--------------------------------------------------------------------
C envoi a N+I mod P
         IF(ANCHOR_REMOTE_SEND%ADD_PROC(I+1)-ANCHOR_REMOTE_SEND%ADD_PROC(I) > 0)THEN
            MSGTYP = 10000 + NSPMD*(LOC_PROC-1) + I
            SIZ = IAD_SEND(I+1)-IAD_SEND(I)
            L = IAD_SEND(I)
            CALL MPI_ISEND(
     S        SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(I),IERROR)
         ENDIF
C--------------------------------------------------------------------
        ENDDO
C
      ENDIF
C
C decompactage
C
      IF (N_ANCHOR_REMOTE > 0) THEN
        DO I = 1, NSPMD
          NB_NOD = ANCHOR_REMOTE%ADD_PROC(I+1)-ANCHOR_REMOTE%ADD_PROC(I)
          IF(NB_NOD>0)THEN
            CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
            L = IAD_RECV(I)
#include        "vectorize.inc"
            DO J=ANCHOR_REMOTE%ADD_PROC(I),ANCHOR_REMOTE%ADD_PROC(I+1)-1
              NOD = ANCHOR_REMOTE%NODE(J)
              A(1,NOD) = A(1,NOD) + RBUF(L)
              A(2,NOD) = A(2,NOD) + RBUF(L+1)
              A(3,NOD) = A(3,NOD) + RBUF(L+2)
              STIFN(NOD) = STIFN(NOD) + RBUF(L+3)          
              L = L + 4
            END DO
C ---
          ENDIF
        END DO
      ENDIF
C
C   wait terminaison isend
C
      IF (N_ANCHOR_REMOTE_SEND > 0) THEN
        DO I = 1, NSPMD
          IF(ANCHOR_REMOTE_SEND%ADD_PROC(I+1)-ANCHOR_REMOTE_SEND%ADD_PROC(I) > 0)THEN
            CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
          ENDIF       
        ENDDO
      ENDIF
C
#endif
      RETURN
      END
