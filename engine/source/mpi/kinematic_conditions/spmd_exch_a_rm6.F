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
Chd|  SPMD_EXCH_A_RM6               source/mpi/kinematic_conditions/spmd_exch_a_rm6.F
Chd|-- called by -----------
Chd|        RMATFORP                      source/materials/mat/mat013/rmatforp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_RM6(
     1    IAD_RBY,FR_RBY6,ICSIZE,RBF6)
C realise le cumul des acc et stifness des noeuds main de rigid bodies
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
      INTEGER ICSIZE, IAD_RBY(*),FR_RBY6(*)
      DOUBLE PRECISION
     .        RBF6(6,6,NRBYM)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,A_AR,NOD,L,I,J,IAD,IRB,
     .        MSGOFF,SIZ,IDEB, LEN,INDEX,NBINDEX,
     .        INDEXI(NSPMD),REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DATA MSGOFF/167/
      PARAMETER (A_AR = 36)           ! 6*6
      DOUBLE PRECISION
     .        RBUF(ICSIZE*A_AR), SBUF(ICSIZE*A_AR)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        IF(LEN.GT.0) THEN
          SIZ = LEN*A_AR
          L=L+1
          INDEXI(L)=I
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(IDEB),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(L),IERROR)
          IDEB = IDEB + SIZ
        ENDIF
      ENDDO
      NBINDEX = L
C
      IDEB = 1
      DO L = 1, NBINDEX
        I = INDEXI(L)
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        IAD = IAD_RBY(I)-1
#include      "vectorize.inc"
        DO J = 1, LEN
          IRB = FR_RBY6(IAD+J)
          SBUF(IDEB)   = RBF6(1,1,IRB)
          SBUF(IDEB+1) = RBF6(1,2,IRB)
          SBUF(IDEB+2) = RBF6(1,3,IRB)
          SBUF(IDEB+3) = RBF6(1,4,IRB)
          SBUF(IDEB+4) = RBF6(1,5,IRB)
          SBUF(IDEB+5) = RBF6(1,6,IRB)
          SBUF(IDEB+6) = RBF6(2,1,IRB)
          SBUF(IDEB+7) = RBF6(2,2,IRB)
          SBUF(IDEB+8) = RBF6(2,3,IRB)
          SBUF(IDEB+9) = RBF6(2,4,IRB)
          SBUF(IDEB+10)= RBF6(2,5,IRB)
          SBUF(IDEB+11)= RBF6(2,6,IRB)
          SBUF(IDEB+12)= RBF6(3,1,IRB)
          SBUF(IDEB+13)= RBF6(3,2,IRB)
          SBUF(IDEB+14)= RBF6(3,3,IRB)
          SBUF(IDEB+15)= RBF6(3,4,IRB)
          SBUF(IDEB+16)= RBF6(3,5,IRB)
          SBUF(IDEB+17)= RBF6(3,6,IRB)
          SBUF(IDEB+18)= RBF6(4,1,IRB)
          SBUF(IDEB+19)= RBF6(4,2,IRB)
          SBUF(IDEB+20)= RBF6(4,3,IRB)
          SBUF(IDEB+21)= RBF6(4,4,IRB)
          SBUF(IDEB+22)= RBF6(4,5,IRB)
          SBUF(IDEB+23)= RBF6(4,6,IRB)
          SBUF(IDEB+24)= RBF6(5,1,IRB)
          SBUF(IDEB+25)= RBF6(5,2,IRB)
          SBUF(IDEB+26)= RBF6(5,3,IRB)
          SBUF(IDEB+27)= RBF6(5,4,IRB)
          SBUF(IDEB+28)= RBF6(5,5,IRB)
          SBUF(IDEB+29)= RBF6(5,6,IRB)
          SBUF(IDEB+30)= RBF6(6,1,IRB)
          SBUF(IDEB+31)= RBF6(6,2,IRB)
          SBUF(IDEB+32)= RBF6(6,3,IRB)
          SBUF(IDEB+33)= RBF6(6,4,IRB)
          SBUF(IDEB+34)= RBF6(6,5,IRB)
          SBUF(IDEB+35)= RBF6(6,6,IRB)
          IDEB = IDEB + A_AR
        ENDDO
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        SIZ = LEN*A_AR
        MSGTYP = MSGOFF 
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_R,INDEX,STATUS,IERROR)
        I = INDEXI(INDEX)
        IDEB = 1+(IAD_RBY(I)-1)*A_AR
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        IAD = IAD_RBY(I)-1
#include      "vectorize.inc"
        DO J = 1, LEN
          IRB = FR_RBY6(IAD+J)
          RBF6(1,1,IRB)= RBF6(1,1,IRB) + RBUF(IDEB)
          RBF6(1,2,IRB)= RBF6(1,2,IRB) + RBUF(IDEB+1)
          RBF6(1,3,IRB)= RBF6(1,3,IRB) + RBUF(IDEB+2)
          RBF6(1,4,IRB)= RBF6(1,4,IRB) + RBUF(IDEB+3)
          RBF6(1,5,IRB)= RBF6(1,5,IRB) + RBUF(IDEB+4)
          RBF6(1,6,IRB)= RBF6(1,6,IRB) + RBUF(IDEB+5)
          RBF6(2,1,IRB)= RBF6(2,1,IRB) + RBUF(IDEB+6)
          RBF6(2,2,IRB)= RBF6(2,2,IRB) + RBUF(IDEB+7)
          RBF6(2,3,IRB)= RBF6(2,3,IRB) + RBUF(IDEB+8)
          RBF6(2,4,IRB)= RBF6(2,4,IRB) + RBUF(IDEB+9)
          RBF6(2,5,IRB)= RBF6(2,5,IRB) + RBUF(IDEB+10)
          RBF6(2,6,IRB)= RBF6(2,6,IRB) + RBUF(IDEB+11)
          RBF6(3,1,IRB)= RBF6(3,1,IRB) + RBUF(IDEB+12)
          RBF6(3,2,IRB)= RBF6(3,2,IRB) + RBUF(IDEB+13)
          RBF6(3,3,IRB)= RBF6(3,3,IRB) + RBUF(IDEB+14)
          RBF6(3,4,IRB)= RBF6(3,4,IRB) + RBUF(IDEB+15)
          RBF6(3,5,IRB)= RBF6(3,5,IRB) + RBUF(IDEB+16)
          RBF6(3,6,IRB)= RBF6(3,6,IRB) + RBUF(IDEB+17)
          RBF6(4,1,IRB)= RBF6(4,1,IRB) + RBUF(IDEB+18)
          RBF6(4,2,IRB)= RBF6(4,2,IRB) + RBUF(IDEB+19)
          RBF6(4,3,IRB)= RBF6(4,3,IRB) + RBUF(IDEB+20)
          RBF6(4,4,IRB)= RBF6(4,4,IRB) + RBUF(IDEB+21)
          RBF6(4,5,IRB)= RBF6(4,5,IRB) + RBUF(IDEB+22)
          RBF6(4,6,IRB)= RBF6(4,6,IRB) + RBUF(IDEB+23)
          RBF6(5,1,IRB)= RBF6(5,1,IRB) + RBUF(IDEB+24)
          RBF6(5,2,IRB)= RBF6(5,2,IRB) + RBUF(IDEB+25)
          RBF6(5,3,IRB)= RBF6(5,3,IRB) + RBUF(IDEB+26)
          RBF6(5,4,IRB)= RBF6(5,4,IRB) + RBUF(IDEB+27)
          RBF6(5,5,IRB)= RBF6(5,5,IRB) + RBUF(IDEB+28)
          RBF6(5,6,IRB)= RBF6(5,6,IRB) + RBUF(IDEB+29)
          RBF6(6,1,IRB)= RBF6(6,1,IRB) + RBUF(IDEB+30)
          RBF6(6,2,IRB)= RBF6(6,2,IRB) + RBUF(IDEB+31)
          RBF6(6,3,IRB)= RBF6(6,3,IRB) + RBUF(IDEB+32)
          RBF6(6,4,IRB)= RBF6(6,4,IRB) + RBUF(IDEB+33)
          RBF6(6,5,IRB)= RBF6(6,5,IRB) + RBUF(IDEB+34)
          RBF6(6,6,IRB)= RBF6(6,6,IRB) + RBUF(IDEB+35)
          IDEB = IDEB + A_AR
        ENDDO
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_S,INDEX,STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END
