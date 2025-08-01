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
!||    debug_chksm_mod   ../engine/source/mpi/output/spmd_flush_accel.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||====================================================================
      MODULE DEBUG_CHKSM_MOD

      CONTAINS
!! \brief /DEBUG/CHKSM engine option, that flushes the Adler32 checksum of the nodal accelration into to the *1.out file 
!||====================================================================
!||    spmd_flush_accel        ../engine/source/mpi/output/spmd_flush_accel.F
!||--- called by ------------------------------------------------------
!||    resol                   ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    double_array_checksum   ../engine/source/mpi/output/node_checksum.F
!||--- uses       -----------------------------------------------------
!||    checksum_mod            ../engine/source/mpi/output/node_checksum.F
!||    spmd_comm_world_mod     ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_FLUSH_ACCEL(NCYCLE,  ISPMD,   NSPMD, NUMNOD,
     .                          NUMNODG, NUMNODM, A,     ITAB,
     .                          WEIGHT,  NODGLOB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE CHECKSUM_MOD , ONLY : DOUBLE_ARRAY_CHECKSUM
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NCYCLE !< current cycle
      INTEGER, INTENT(IN) :: ISPMD !< MPI rank
      INTEGER, INTENT(IN) :: NSPMD !< number of MPI processes
      INTEGER, INTENT(IN) :: NUMNOD !< number of nodes on the current MPI process 
      INTEGER, INTENT(IN) :: NUMNODG !< total number of nodes 
      INTEGER, INTENT(IN) :: NUMNODM !< maximum number of nodes over the MPI processes 
      INTEGER, INTENT(IN) :: ITAB(NUMNOD) !< user node id
      INTEGER, INTENT(IN) :: WEIGHT(NUMNOD) !< weight of the node (1 on the processor that assembles the forces, 0 elsewhere)
      INTEGER, INTENT(IN) :: NODGLOB(NUMNOD) !< global node id 
      my_real, INTENT(IN) :: A(3,NUMNOD) !< nodal acceleration
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
#endif

      INTEGER :: MSGOFF,MSGOFF0,MSGTYP !< MPI messages
      INTEGER :: I,K,N,NODE_GLOBAL_ID !< loop indices
      INTEGER, DIMENSION(:), ALLOCATABLE :: NODES_TO_SEND
      DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: AGLOB,NODES_TO_RECV
      INTEGER :: CHECKSUM !< checksum of the nodal acceleration
      DATA MSGOFF0/176/
      DATA MSGOFF/177/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      IF(ISPMD == 0) THEN
        ALLOCATE(NODES_TO_RECV(4,NUMNODG))
      ENDIF
 
      ALLOCATE(NODES_TO_SEND(MAX(NUMNOD,NUMNODM)))
      ALLOCATE(AGLOB(4,MAX(NUMNODM,NUMNOD)))

      IF (ISPMD/=0) THEN
#ifdef MPI
        N = 0
        DO I = 1, NUMNOD
          IF (WEIGHT(I)==1) THEN
            N = N+1
            NODES_TO_SEND(N) = NODGLOB(I)
            AGLOB(1,N) = ITAB(I)
            AGLOB(2,N) = A(1,I)
            AGLOB(3,N) = A(2,I)
            AGLOB(4,N) = A(3,I)
          END IF
        END DO
        MSGTYP=MSGOFF0
        CALL MPI_SEND(NODES_TO_SEND,N,MPI_INTEGER,
     .                0,MSGTYP,
     .                SPMD_COMM_WORLD,IERROR)
        MSGTYP=MSGOFF
        CALL MPI_SEND(AGLOB,4*N,MPI_DOUBLE_PRECISION,
     .                0,MSGTYP,
     .                SPMD_COMM_WORLD,IERROR)
#endif
      ELSE ! ISPMD == 0

        DO I=1,NUMNOD
          IF (WEIGHT(I)==1) THEN
            NODE_GLOBAL_ID = NODGLOB(I)
            NODES_TO_RECV(1,NODE_GLOBAL_ID) = ITAB(I)
            NODES_TO_RECV(2,NODE_GLOBAL_ID) =  A(1,I)
            NODES_TO_RECV(3,NODE_GLOBAL_ID) =  A(2,I)
            NODES_TO_RECV(4,NODE_GLOBAL_ID) =  A(3,I)
          ENDIF
        ENDDO
#ifdef MPI
        DO K=2,NSPMD
          MSGTYP=MSGOFF0
          CALL MPI_RECV(NODES_TO_SEND,NUMNODM,MPI_INTEGER,
     .                  K-1,MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,IERROR)
          CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,N,ierror)
          MSGTYP=MSGOFF
          CALL MPI_RECV(AGLOB,4*N,MPI_DOUBLE_PRECISION,
     .                  K-1,MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,IERROR)
          DO I=1,N
            NODE_GLOBAL_ID = NODES_TO_SEND(I)
            NODES_TO_RECV(1,NODE_GLOBAL_ID) = AGLOB(1,I)
            NODES_TO_RECV(2,NODE_GLOBAL_ID) = AGLOB(2,I)
            NODES_TO_RECV(3,NODE_GLOBAL_ID) = AGLOB(3,I)
            NODES_TO_RECV(4,NODE_GLOBAL_ID) = AGLOB(4,I)
          ENDDO

        END DO
#endif
        CHECKSUM = DOUBLE_ARRAY_CHECKSUM(NODES_TO_RECV,NUMNODG,4)
        WRITE(IOUT,*) NCYCLE, "CHECKSUM:",CHECKSUM

      ENDIF
      IF(ALLOCATED(NODES_TO_SEND))  DEALLOCATE(NODES_TO_SEND)
      IF(ALLOCATED(AGLOB))   DEALLOCATE(AGLOB)
      IF(ALLOCATED(NODES_TO_RECV)) DEALLOCATE(NODES_TO_RECV)
      RETURN
      END
      END MODULE DEBUG_CHKSM_MOD
