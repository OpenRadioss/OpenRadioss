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
C
Chd|====================================================================
Chd|  SPMD_EXCH_A_IB_PON            source/mpi/kinematic_conditions/spmd_exch_a_ib_pon.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_IB_PON(
     1   LBVRS,FR_RBM  ,IAD_RBM,FSKYRBM,LSEND ,
     2   LRECV,LSKYRBMG)
C realise le cumul des acc et stifness des noeuds main
C  de rigid bodies mous en parit/on
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LSEND ,LRECV, LSKYRBMG, FR_RBM(3,*),IAD_RBM(4,*)
      my_real
     .        LBVRS(21,*), FSKYRBM(21,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,A_AR,N,L,I,J,II,K,M,JJ, LEN,
     .        MSGTYP,MSGOFF,MSGOFF2,SIZ,IDEBR,IDEBS,
     .        IERROR, NBIRECV, NBISEND, INDEX, NBRBY, NBNOD,
     .        PMAIN, IDEB, LENS, LENR,
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD), REQ_S(NSPMD),
     .        IRINDEX(NSPMD), ISINDEX(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),IAD_STMP(NSPMD)
      DATA MSGOFF/174/
      DATA MSGOFF2/175/
      PARAMETER(A_AR = 21)
      my_real
     .        SBUF(A_AR*LSEND), RBUF(A_AR*LRECV),
     .        FSKYRBMG(A_AR-1,LSKYRBMG)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      NBIRECV = 0
      NBISEND = 0
      IDEBR = 1
      IDEBS = 1
      DO I = 1, NSPMD
        IAD_RECV(I) = IDEBR
        IF(IAD_RBM(2,I)>0) THEN
          MSGTYP = MSGOFF
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          SIZ = IAD_RBM(2,I)*A_AR
          CALL MPI_IRECV(
     S       RBUF(IDEBR),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G       MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEBR = IDEBR + SIZ
        ENDIF
        IAD_SEND(I) = IDEBS
        IF(IAD_RBM(1,I)>0) THEN
          NBISEND = NBISEND + 1
          ISINDEX(NBISEND) = I
          SIZ = IAD_RBM(1,I)*A_AR
          IDEBS = IDEBS + SIZ
          IAD_STMP(I)=IAD_SEND(I)
        ENDIF
      ENDDO
      IAD_RECV(NSPMD+1) = IDEBR
C
      IDEB = 0
      DO N = 1, NIBVEL
        PMAIN = FR_RBM(3,N)
        NBNOD = FR_RBM(1,N)
        IF(PMAIN>0.AND.LOC_PROC/=PMAIN) THEN
          L = IAD_STMP(PMAIN)
          DO K = 1, NBNOD
            SBUF(L)   = FSKYRBM(1,IDEB+K)
            SBUF(L+1) = FSKYRBM(2,IDEB+K)
            SBUF(L+2) = FSKYRBM(3,IDEB+K)
            SBUF(L+3) = FSKYRBM(4,IDEB+K)
            SBUF(L+4) = FSKYRBM(5,IDEB+K)
            SBUF(L+5) = FSKYRBM(6,IDEB+K)
            SBUF(L+6) = FSKYRBM(7,IDEB+K)
            SBUF(L+7) = FSKYRBM(8,IDEB+K)
            SBUF(L+8) = FSKYRBM(9,IDEB+K)
            SBUF(L+9) = FSKYRBM(10,IDEB+K)
            SBUF(L+10)= FSKYRBM(11,IDEB+K)
            SBUF(L+11)= FSKYRBM(12,IDEB+K)
            SBUF(L+12)= FSKYRBM(13,IDEB+K)
            SBUF(L+13)= FSKYRBM(14,IDEB+K)
            SBUF(L+14)= FSKYRBM(15,IDEB+K)
            SBUF(L+15)= FSKYRBM(16,IDEB+K)
            SBUF(L+16)= FSKYRBM(17,IDEB+K)
            SBUF(L+17)= FSKYRBM(18,IDEB+K)
            SBUF(L+18)= FSKYRBM(19,IDEB+K)
            SBUF(L+19)= FSKYRBM(20,IDEB+K)
            SBUF(L+20)= FSKYRBM(21,IDEB+K)
            L = L + A_AR
          ENDDO
          IAD_STMP(PMAIN)=L

        ELSE
C si proc main : stockage direct
          DO K = 1, NBNOD
            II = NINT(FSKYRBM(1,IDEB+K))
            FSKYRBMG(1,II) = FSKYRBM(2,IDEB+K)
            FSKYRBMG(2,II) = FSKYRBM(3,IDEB+K)
            FSKYRBMG(3,II) = FSKYRBM(4,IDEB+K)
            FSKYRBMG(4,II) = FSKYRBM(5,IDEB+K)
            FSKYRBMG(5,II) = FSKYRBM(6,IDEB+K)
            FSKYRBMG(6,II) = FSKYRBM(7,IDEB+K)
            FSKYRBMG(7,II) = FSKYRBM(8,IDEB+K)
            FSKYRBMG(8,II) = FSKYRBM(9,IDEB+K)
            FSKYRBMG(9,II) = FSKYRBM(10,IDEB+K)
            FSKYRBMG(10,II)= FSKYRBM(11,IDEB+K)
            FSKYRBMG(11,II)= FSKYRBM(12,IDEB+K)
            FSKYRBMG(12,II)= FSKYRBM(13,IDEB+K)
            FSKYRBMG(13,II)= FSKYRBM(14,IDEB+K)
            FSKYRBMG(14,II)= FSKYRBM(15,IDEB+K)
            FSKYRBMG(15,II)= FSKYRBM(16,IDEB+K)
            FSKYRBMG(16,II)= FSKYRBM(17,IDEB+K)
            FSKYRBMG(17,II)= FSKYRBM(18,IDEB+K)
            FSKYRBMG(18,II)= FSKYRBM(19,IDEB+K)
            FSKYRBMG(19,II)= FSKYRBM(20,IDEB+K)
            FSKYRBMG(20,II)= FSKYRBM(21,IDEB+K)
          ENDDO
        ENDIF
       IDEB = IDEB + NBNOD
      ENDDO
C
      DO L = 1, NBISEND
        I = ISINDEX(L)
        SIZ = IAD_STMP(I)-IAD_SEND(I)
        IDEBS = IAD_SEND(I)
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(IDEBS),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(I),IERROR)
      ENDDO
C
C Remplissage de la matrice FSKYRBKG par proc remote
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)
        DO K = 1, IAD_RBM(2,I)
          JJ =  NINT(RBUF(IDEB+(K-1)*A_AR))
          FSKYRBMG(1,JJ) = RBUF(IDEB+(K-1)*A_AR+1)
          FSKYRBMG(2,JJ) = RBUF(IDEB+(K-1)*A_AR+2)
          FSKYRBMG(3,JJ) = RBUF(IDEB+(K-1)*A_AR+3)
          FSKYRBMG(4,JJ) = RBUF(IDEB+(K-1)*A_AR+4)
          FSKYRBMG(5,JJ) = RBUF(IDEB+(K-1)*A_AR+5)
          FSKYRBMG(6,JJ) = RBUF(IDEB+(K-1)*A_AR+6)
          FSKYRBMG(7,JJ) = RBUF(IDEB+(K-1)*A_AR+7)
          FSKYRBMG(8,JJ) = RBUF(IDEB+(K-1)*A_AR+8)
          FSKYRBMG(9,JJ) = RBUF(IDEB+(K-1)*A_AR+9)
          FSKYRBMG(10,JJ)= RBUF(IDEB+(K-1)*A_AR+10)
          FSKYRBMG(11,JJ)= RBUF(IDEB+(K-1)*A_AR+11)
          FSKYRBMG(12,JJ)= RBUF(IDEB+(K-1)*A_AR+12)
          FSKYRBMG(13,JJ)= RBUF(IDEB+(K-1)*A_AR+13)
          FSKYRBMG(14,JJ)= RBUF(IDEB+(K-1)*A_AR+14)
          FSKYRBMG(15,JJ)= RBUF(IDEB+(K-1)*A_AR+15)
          FSKYRBMG(16,JJ)= RBUF(IDEB+(K-1)*A_AR+16)
          FSKYRBMG(17,JJ)= RBUF(IDEB+(K-1)*A_AR+17)
          FSKYRBMG(18,JJ)= RBUF(IDEB+(K-1)*A_AR+18)
          FSKYRBMG(19,JJ)= RBUF(IDEB+(K-1)*A_AR+19)
          FSKYRBMG(20,JJ)= RBUF(IDEB+(K-1)*A_AR+20)
        ENDDO
      ENDDO
C
C Calcul des forces aux noeuds main sur proc main
C
      IDEB = 0
      DO N = 1, NIBVEL
        IF(LOC_PROC==ABS(FR_RBM(3,N)))THEN
          NBNOD = FR_RBM(2,N)
          DO II = 1, NBNOD
            LBVRS(1,N) =  LBVRS(1,N) + FSKYRBMG(1,IDEB+II)
            LBVRS(2,N) =  LBVRS(2,N) + FSKYRBMG(2,IDEB+II)
            LBVRS(3,N) =  LBVRS(3,N) + FSKYRBMG(3,IDEB+II)
            LBVRS(4,N) =  LBVRS(4,N) + FSKYRBMG(4,IDEB+II)
            LBVRS(5,N) =  LBVRS(5,N) + FSKYRBMG(5,IDEB+II)
            LBVRS(6,N) =  LBVRS(6,N) + FSKYRBMG(6,IDEB+II)
            LBVRS(7,N) =  LBVRS(7,N) + FSKYRBMG(7,IDEB+II)
            LBVRS(8,N) =  LBVRS(8,N) + FSKYRBMG(8,IDEB+II)
            LBVRS(9,N) =  LBVRS(9,N) + FSKYRBMG(9,IDEB+II)
            LBVRS(10,N)=  LBVRS(10,N)+ FSKYRBMG(10,IDEB+II)
            LBVRS(11,N)=  LBVRS(11,N)+ FSKYRBMG(11,IDEB+II)
            LBVRS(12,N)=  LBVRS(12,N)+ FSKYRBMG(12,IDEB+II)
            LBVRS(13,N)=  LBVRS(13,N)+ FSKYRBMG(13,IDEB+II)
            LBVRS(14,N)=  LBVRS(14,N)+ FSKYRBMG(14,IDEB+II)
            LBVRS(15,N)=  LBVRS(15,N)+ FSKYRBMG(15,IDEB+II)
            LBVRS(16,N)=  LBVRS(16,N)+ FSKYRBMG(16,IDEB+II)
            LBVRS(17,N)=  LBVRS(17,N)+ FSKYRBMG(17,IDEB+II)
            LBVRS(18,N)=  LBVRS(18,N)+ FSKYRBMG(18,IDEB+II)
            LBVRS(19,N)=  LBVRS(19,N)+ FSKYRBMG(19,IDEB+II)
            LBVRS(20,N)=  LBVRS(20,N)+ FSKYRBMG(20,IDEB+II)
          END DO
          IDEB = IDEB + NBNOD
        END IF
      END DO
      DO L = 1, NBISEND
        I = ISINDEX(L)
        CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
C
C Envois des forces des noeuds main aux autres procs
C
      NBIRECV = 0
      IDEBR = 1
      DO I = 1, NSPMD
        IAD_RECV(I) = IDEBR
        IF(IAD_RBM(4,I)>0) THEN
          MSGTYP = MSGOFF2 
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          SIZ = IAD_RBM(4,I)*A_AR
          CALL MPI_IRECV(
     S       RBUF(IDEBR),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G       MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEBR = IDEBR + SIZ
        ENDIF
      ENDDO
C
      IF(IAD_RBM(3,NSPMD+1)>0) THEN
        L = 0
        DO N = 1, NIBVEL
          IF(LOC_PROC==FR_RBM(3,N)) THEN
            SBUF(L+1) = N
            SBUF(L+2) = LBVRS(1,N)
            SBUF(L+3) = LBVRS(2,N)
            SBUF(L+4) = LBVRS(3,N)
            SBUF(L+5) = LBVRS(4,N)
            SBUF(L+6) = LBVRS(5,N)
            SBUF(L+7) = LBVRS(6,N)
            SBUF(L+8) = LBVRS(7,N)
            SBUF(L+9) = LBVRS(8,N)
            SBUF(L+10) = LBVRS(9,N)
            SBUF(L+11) = LBVRS(10,N)
            SBUF(L+12) = LBVRS(11,N)
            SBUF(L+13) = LBVRS(12,N)
            SBUF(L+14) = LBVRS(13,N)
            SBUF(L+15) = LBVRS(14,N)
            SBUF(L+16) = LBVRS(15,N)
            SBUF(L+17) = LBVRS(16,N)
            SBUF(L+18) = LBVRS(17,N)
            SBUF(L+19) = LBVRS(18,N)
            SBUF(L+20) = LBVRS(19,N)
            SBUF(L+21) = LBVRS(20,N)
            L = L + A_AR
          END IF
        END DO
C
        NBISEND = 0
        DO I = 1, NSPMD
          IF(IAD_RBM(3,I)>0) THEN
            MSGTYP = MSGOFF2 
            NBISEND = NBISEND + 1
            ISINDEX(NBISEND) = I
            CALL MPI_ISEND(
     S        SBUF,L,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(I),IERROR)
          END IF
        END DO
      END IF
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        L = IAD_RECV(I)
        NBNOD = IAD_RBM(4,I)
        DO J = 1, NBNOD
          N = NINT(RBUF(L))
C rby non utile
          IF(FR_RBM(2,N)/=0)THEN
            LBVRS(1,N) = RBUF(L+1)
            LBVRS(2,N) = RBUF(L+2)
            LBVRS(3,N) = RBUF(L+3)
            LBVRS(4,N) = RBUF(L+4)
            LBVRS(5,N) = RBUF(L+5)
            LBVRS(6,N) = RBUF(L+6)
            LBVRS(7,N) = RBUF(L+7)
            LBVRS(8,N) = RBUF(L+8)
            LBVRS(9,N) = RBUF(L+9)
            LBVRS(10,N)= RBUF(L+10)
            LBVRS(11,N)= RBUF(L+11)
            LBVRS(12,N)= RBUF(L+12)
            LBVRS(13,N)= RBUF(L+13)
            LBVRS(14,N)= RBUF(L+14)
            LBVRS(15,N)= RBUF(L+15)
            LBVRS(16,N)= RBUF(L+16)
            LBVRS(17,N)= RBUF(L+17)
            LBVRS(18,N)= RBUF(L+18)
            LBVRS(19,N)= RBUF(L+19)
            LBVRS(20,N)= RBUF(L+20)
          END IF
          L = L + A_AR
        END DO
      END DO
C
      DO N = 1, NIBVEL
        LBVRS(21,N) = FR_RBM(2,N)
      END DO
C
      DO L = 1, NBISEND
        I = ISINDEX(L)
        CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      END DO
C
#endif
      RETURN
      END
