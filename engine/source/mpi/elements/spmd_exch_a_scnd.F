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
!||    spmd_exch_a_scnd      ../engine/source/mpi/elements/spmd_exch_a_scnd.F
!||--- called by ------------------------------------------------------
!||    s10cndamp             ../engine/source/elements/solid/solide10/s10cndf.F
!||    s10cndf1              ../engine/source/elements/solid/solide10/s10cndf.F
!||    s10cndf2              ../engine/source/elements/solid/solide10/s10cndf.F
!||    s10cnidamp            ../engine/source/elements/solid/solide10/s10cndf.F
!||    s10cnistat            ../engine/source/elements/solid/solide10/s10cndf.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_EXCH_A_SCND(
     1            A    ,STIFN ,FR_CDNM,IAD_CDNM, LCOMM,ISIZE)
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
      INTEGER LCOMM, ISIZE, FR_CDNM(*), IAD_CDNM(*)
      my_real
     .        A(3,*),STIFN(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD)
      DATA MSGOFF/1178/
      my_real
     .        SBUF(LCOMM*ISIZE),RBUF(LCOMM*ISIZE)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IDEB = 1
      L = 0
c      ISIZE = 3
      DO I = 1, NSPMD
        LEN = IAD_CDNM(I+1)-IAD_CDNM(I)
        IF(LEN>0) THEN
          SIZ = LEN*ISIZE 
          L=L+1
          INDEXI(L)=I
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      SPMD_COMM_WORLD,REQ_R(L),IERROR)
          IDEB = IDEB + SIZ
        ENDIF
      ENDDO
      NBINDEX = L
C
      IDEB = 1
      DO L = 1, NBINDEX
        I = INDEXI(L)
        LEN = IAD_CDNM(I+1) - IAD_CDNM(I)
        IAD = IAD_CDNM(I)-1
        DO J = 1, LEN
         NOD = FR_CDNM(IAD+J)
         SBUF(IDEB)   = A(1,NOD)
         SBUF(IDEB+1) = A(2,NOD)
         SBUF(IDEB+2) = A(3,NOD)
         SBUF(IDEB+3) = STIFN(NOD)
         IDEB = IDEB + ISIZE
        ENDDO
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        LEN = IAD_CDNM(I+1)-IAD_CDNM(I)
        SIZ = LEN*ISIZE
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    SPMD_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_R,INDEX,STATUS,IERROR)
        I = INDEXI(INDEX)
        IDEB = 1+(IAD_CDNM(I)-1)*ISIZE
        LEN = IAD_CDNM(I+1)-IAD_CDNM(I)
        IAD = IAD_CDNM(I)-1
        DO J = 1, LEN
         NOD = FR_CDNM(IAD+J)
         A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
         A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
         A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
         STIFN(NOD) = STIFN(NOD) + RBUF(IDEB+3)
         IDEB = IDEB + ISIZE
        ENDDO
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_S,INDEX,STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END SUBROUTINE

