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
!||    spmd_sd_cj_1          ../engine/source/mpi/kinematic_conditions/spmd_sd_cj_1.F
!||--- called by ------------------------------------------------------
!||    sms_cjoint_1          ../engine/source/ams/sms_cjoint.F
!||    sms_cjoint_2          ../engine/source/ams/sms_cjoint.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_SD_CJ_1(A,LJOINT,FR_CJ,IADCJ,ICSIZE,
     .                        TAG_LNK_SMS)
C envoi de p0 vers les autres procs de A
C apres traitement des joints cylindriques
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
      INTEGER ICSIZE, LJOINT(*),FR_CJ(*), IADCJ(NSPMD+1,*),
     .        TAG_LNK_SMS(*)
      my_real
     .        A(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,MSGOFF,IERROR,LOC_PROC,NOD,L,I,J,N,
     .        IDEB,IAD,SIZ,A_AR,NBINDEX,
     .        INDEXI(NSPMD),REQ_S(NSPMD),STATUS(MPI_STATUS_SIZE),
     .        LEN(NSPMD)
      PARAMETER (A_AR = 3)
      my_real
     .        BUF(ICSIZE*A_AR)
      DATA MSGOFF/103/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      IF (LOC_PROC==1) THEN
        L = 0
        IDEB = 1

        DO I = 2, NSPMD
          LEN(I)=0
          DO N=1,NJOINT
           IF(TAG_LNK_SMS(N)/=0)
     .       LEN(I) = LEN(I)+IADCJ(I+1,N)-IADCJ(I,N)
          END DO
          IF(LEN(I)>0) THEN
            L=L+1
            INDEXI(L)=I
            DO N=1,NJOINT
              IF(TAG_LNK_SMS(N)==0)CYCLE
              DO J = IADCJ(I,N), IADCJ(I+1,N)-1
                NOD = FR_CJ(J)
                BUF(IDEB  )  = A(1,NOD)
                BUF(IDEB+1)  = A(2,NOD)
                BUF(IDEB+2)  = A(3,NOD)
                IDEB = IDEB + A_AR
              END DO
            END DO
          END IF
        END DO
        NBINDEX = L
C
        IDEB = 1

        DO L=1,NBINDEX
         I = INDEXI(L)
         SIZ = LEN(I)*A_AR
         MSGTYP = MSGOFF
         CALL MPI_ISEND(
     S     BUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G     SPMD_COMM_WORLD,REQ_S(L),IERROR)
         IDEB = IDEB + SIZ
        END DO
C
        DO L=1,NBINDEX
          CALL MPI_WAITANY(NBINDEX,REQ_S,I,STATUS,IERROR)
        ENDDO
      ELSEIF(ICSIZE>0) THEN
C proc <> 0
        IDEB = 1
        SIZ = ICSIZE*A_AR
        MSGTYP = MSGOFF
        CALL MPI_RECV(
     S    BUF,SIZ,REAL,IT_SPMD(1),MSGTYP,
     G    SPMD_COMM_WORLD,STATUS,IERROR)
        DO N = 1, NJOINT
          IF(TAG_LNK_SMS(N)==0)CYCLE
          DO J = IADCJ(1,N),IADCJ(NSPMD+1,N)-1
            NOD = FR_CJ(J)
            A(1,NOD) = BUF(IDEB)
            A(2,NOD) = BUF(IDEB+1)
            A(3,NOD) = BUF(IDEB+2)
            IDEB = IDEB + A_AR
          END DO
        END DO
      END IF
C
#endif
      RETURN
      END
