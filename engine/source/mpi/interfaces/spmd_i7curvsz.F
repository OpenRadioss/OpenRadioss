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
!||    spmd_i7curvsz         ../engine/source/mpi/interfaces/spmd_i7curvsz.F
!||--- called by ------------------------------------------------------
!||    i7mainf               ../engine/source/interfaces/int07/i7mainf.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_I7CURVSZ(NRTM  ,IRECT ,NUMNOD,IAD_ELEM,FR_ELEM,
     2                         ISDSIZ,IRCSIZ,ITAG  ,LENR    ,LENT   ,
     3                         MAXCC )
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
      INTEGER NRTM,NUMNOD,LENR,LENT,MAXCC,
     .        IRECT(4,NRTM),ITAG(*),
     .        IAD_ELEM(2,*), FR_ELEM(*), ISDSIZ(*), IRCSIZ(*)
C     REAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I ,J ,N1, N2, N3, N4, IERROR, IAD, SIZ, NB,
     .        MSGTYP, LOC_PROC, MSGOFF,
     .        IBUFS(LENR), IBUFR(LENR),
     .        STATUS(MPI_STATUS_SIZE),REQ_R(NSPMD)
      DATA MSGOFF/193/

C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
C
      IAD = 1
      DO I=1,NSPMD
        IRCSIZ(I) = 0
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ>0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      IBUFR(IAD),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      SPMD_COMM_WORLD,REQ_R(I),IERROR)
            IAD = IAD + SIZ
        ENDIF
      END DO
      DO N1=1,NUMNOD
        ITAG(N1) = 0
      END DO

      DO I=1,NRTM
        N1 = IRECT(1,I)
        N2 = IRECT(2,I)
        N3 = IRECT(3,I)
        N4 = IRECT(4,I)
        ITAG(N1) = ITAG(N1) + 1
        ITAG(N2) = ITAG(N2) + 1
        ITAG(N3) = ITAG(N3) + 1
        ITAG(N4) = ITAG(N4) + 1
      END DO
      DO I=1,NSPMD
        ISDSIZ(I) = 0
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ>0)THEN
          IAD = 1
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N1 = FR_ELEM(J)
            ISDSIZ(I) = ISDSIZ(I) + ITAG(N1)
            IBUFS(IAD) = ITAG(N1)
            IAD = IAD + 1
          END DO
          MSGTYP = MSGOFF
          CALL MPI_SEND(
     S      IBUFS,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G      SPMD_COMM_WORLD,IERROR)
        END IF
      END DO
C
      ISDSIZ(NSPMD+1) = 0
      IRCSIZ(NSPMD+1) = 0
      IAD = 0
      DO I = 1, NSPMD
        SIZ = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(SIZ>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            N1 = FR_ELEM(J)
            IAD = IAD + 1
            NB = IBUFR(IAD)
            IRCSIZ(I) = IRCSIZ(I) + NB
            ITAG(N1) = ITAG(N1) + NB
          END DO
          ISDSIZ(NSPMD+1) = ISDSIZ(NSPMD+1) + ISDSIZ(I)
          IRCSIZ(NSPMD+1) = IRCSIZ(NSPMD+1) + IRCSIZ(I)
        END IF
      END DO
C
      LENT = 0
      MAXCC = 0
      DO N1=1,NUMNOD
        LENT = LENT + ITAG(N1)
        MAXCC = MAX(MAXCC,ITAG(N1))
      END DO
C
#endif
      RETURN
      END
