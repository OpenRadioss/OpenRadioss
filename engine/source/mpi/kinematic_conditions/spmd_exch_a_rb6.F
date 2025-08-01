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
C
!||====================================================================
!||    spmd_exch_a_rb6       ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
!||--- called by ------------------------------------------------------
!||    rbyfor                ../engine/source/constraints/general/rbody/rbyfor.F
!||    sms_encin_2           ../engine/source/ams/sms_encin_2.F
!||    sms_mass_scale_2      ../engine/source/ams/sms_mass_scale_2.F
!||    sms_pcg               ../engine/source/ams/sms_pcg.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod           ../engine/share/message_module/message_mod.F
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_EXCH_A_RB6(NRBDIM,IAD_RBY,FR_RBY6,ICSIZE,RBF6)
C     make summation of acc and stiffness for main nodes of rigid bodies
C     and make summation of other quantities for AMS
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRBDIM,ICSIZE, IAD_RBY(*),FR_RBY6(*)
      DOUBLE PRECISION
     .        RBF6(8,6,NRBYKIN)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,A_AR,NOD,L,I,J,K,IAD,IRB,
     .        MSGOFF,SIZ,IDEB, LEN,INDEX,NBINDEX,
     .        INDEXI(NSPMD),REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DATA MSGOFF/165/
      DOUBLE PRECISION
     .        RBUF(ICSIZE*6*NRBDIM), SBUF(ICSIZE*6*NRBDIM)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      A_AR=6*NRBDIM

      LOC_PROC = ISPMD + 1
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        IF(LEN>0) THEN
          SIZ = LEN*A_AR
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
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        IAD = IAD_RBY(I)-1
#include      "vectorize.inc"
        DO J = 1, LEN
          IRB = FR_RBY6(IAD+J)
          DO K = 1, NRBDIM
            SBUF(IDEB)   = RBF6(K,1,IRB)
            SBUF(IDEB+1) = RBF6(K,2,IRB)
            SBUF(IDEB+2) = RBF6(K,3,IRB)
            SBUF(IDEB+3) = RBF6(K,4,IRB)
            SBUF(IDEB+4) = RBF6(K,5,IRB)
            SBUF(IDEB+5) = RBF6(K,6,IRB)
            IDEB=IDEB+6
          ENDDO
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
     G    SPMD_COMM_WORLD,REQ_S(L),IERROR)
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
          DO K = 1, NRBDIM
            RBF6(K,1,IRB)= RBF6(K,1,IRB) + RBUF(IDEB)
            RBF6(K,2,IRB)= RBF6(K,2,IRB) + RBUF(IDEB+1)
            RBF6(K,3,IRB)= RBF6(K,3,IRB) + RBUF(IDEB+2)
            RBF6(K,4,IRB)= RBF6(K,4,IRB) + RBUF(IDEB+3)
            RBF6(K,5,IRB)= RBF6(K,5,IRB) + RBUF(IDEB+4)
            RBF6(K,6,IRB)= RBF6(K,6,IRB) + RBUF(IDEB+5)
            IDEB = IDEB + 6
          ENDDO
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
C
!||====================================================================
!||    spmd_exch_a_rb6_vrel   ../engine/source/mpi/kinematic_conditions/spmd_exch_a_rb6.F
!||--- called by ------------------------------------------------------
!||    rbyfor                 ../engine/source/constraints/general/rbody/rbyfor.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    message_mod            ../engine/share/message_module/message_mod.F
!||    spmd_comm_world_mod    ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_EXCH_A_RB6_VREL(NRBDIM,IAD_RBY,FR_RBY6,ICSIZE,RBY6,
     .                                RBY6_C,SIZE_RBY6_C)
C     make summation of acc, stiffness for main nodes of rigid bodies
C     make summation of damping coefficient for /DAMP/VREL
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NRBDIM,ICSIZE, IAD_RBY(*),FR_RBY6(*),SIZE_RBY6_C
      DOUBLE PRECISION
     .        RBY6(8,6,NRBYKIN),RBY6_C(2,6,SIZE_RBY6_C)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,A_AR,NOD,L,I,J,K,IAD,IRB,
     .        MSGOFF,SIZ,IDEB, LEN,INDEX,NBINDEX,
     .        INDEXI(NSPMD),REQ_R(NSPMD),REQ_S(NSPMD)
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DATA MSGOFF/165/
      DOUBLE PRECISION
     .        RBUF(ICSIZE*6*NRBDIM), SBUF(ICSIZE*6*NRBDIM)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      A_AR=6*NRBDIM

      LOC_PROC = ISPMD + 1
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        IF(LEN>0) THEN
          SIZ = LEN*A_AR
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
        LEN = IAD_RBY(I+1)-IAD_RBY(I)
        IAD = IAD_RBY(I)-1
#include      "vectorize.inc"
        DO J = 1, LEN
          IRB = FR_RBY6(IAD+J)
          DO K = 1, NRBDIM-2
            SBUF(IDEB)   = RBY6(K,1,IRB)
            SBUF(IDEB+1) = RBY6(K,2,IRB)
            SBUF(IDEB+2) = RBY6(K,3,IRB)
            SBUF(IDEB+3) = RBY6(K,4,IRB)
            SBUF(IDEB+4) = RBY6(K,5,IRB)
            SBUF(IDEB+5) = RBY6(K,6,IRB)
            IDEB=IDEB+6
          ENDDO
          DO K = 1, 2
            SBUF(IDEB)   = RBY6_C(K,1,IRB)
            SBUF(IDEB+1) = RBY6_C(K,2,IRB)
            SBUF(IDEB+2) = RBY6_C(K,3,IRB)
            SBUF(IDEB+3) = RBY6_C(K,4,IRB)
            SBUF(IDEB+4) = RBY6_C(K,5,IRB)
            SBUF(IDEB+5) = RBY6_C(K,6,IRB)
            IDEB=IDEB+6
          ENDDO
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
     G    SPMD_COMM_WORLD,REQ_S(L),IERROR)
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
          DO K = 1, NRBDIM-2
            RBY6(K,1,IRB)= RBY6(K,1,IRB) + RBUF(IDEB)
            RBY6(K,2,IRB)= RBY6(K,2,IRB) + RBUF(IDEB+1)
            RBY6(K,3,IRB)= RBY6(K,3,IRB) + RBUF(IDEB+2)
            RBY6(K,4,IRB)= RBY6(K,4,IRB) + RBUF(IDEB+3)
            RBY6(K,5,IRB)= RBY6(K,5,IRB) + RBUF(IDEB+4)
            RBY6(K,6,IRB)= RBY6(K,6,IRB) + RBUF(IDEB+5)
            IDEB = IDEB + 6
          ENDDO
          DO K = 1, 2
            RBY6_C(K,1,IRB)= RBY6_C(K,1,IRB) + RBUF(IDEB)
            RBY6_C(K,2,IRB)= RBY6_C(K,2,IRB) + RBUF(IDEB+1)
            RBY6_C(K,3,IRB)= RBY6_C(K,3,IRB) + RBUF(IDEB+2)
            RBY6_C(K,4,IRB)= RBY6_C(K,4,IRB) + RBUF(IDEB+3)
            RBY6_C(K,5,IRB)= RBY6_C(K,5,IRB) + RBUF(IDEB+4)
            RBY6_C(K,6,IRB)= RBY6_C(K,6,IRB) + RBUF(IDEB+5)
            IDEB = IDEB + 6
          ENDDO
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
