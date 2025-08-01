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
!||    spmd_velvec2          ../engine/source/mpi/anim/spmd_velvec2.F
!||--- called by ------------------------------------------------------
!||    velvec2               ../engine/source/output/anim/generate/velvec.F
!||--- calls      -----------------------------------------------------
!||    write_r_c             ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_VELVEC2(V,NODGLOB,RBUF,NUMPOG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),RBUF,NUMPOG

      my_real 
     .   V(3,*)

C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,J,K,NG,NREC,MSGOFF2
      my_real
     .     R1,R2,R3

      INTEGER, DIMENSION(:), ALLOCATABLE ::  NGLOBI
      my_real,
     *    DIMENSION(:,:),ALLOCATABLE :: VRECV
      REAL, DIMENSION(:,:),ALLOCATABLE :: VTOTAL


      DATA MSGOFF/7024/
      DATA MSGOFF2/7025/
C-----------------------------------------------
       ALLOCATE(NGLOBI(RBUF))
       ALLOCATE(VRECV(3,RBUF))
       ALLOCATE(VTOTAL(3,NUMPOG))

       IF (ISPMD/=0) THEN
C   on envoie le nodglob
         MSGTYP = MSGOFF

         CALL MPI_SEND(NODGLOB,NUMNOD,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

         MSGTYP = MSGOFF2
         CALL MPI_SEND(V,3*NUMNOD,REAL,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)

       ELSE
          DO I=1,NUMNODG
            VTOTAL(1,I) = ZERO
            VTOTAL(2,I) = ZERO
            VTOTAL(3,I) = ZERO
          ENDDO

          DO I=1,NUMNOD
            K = NODGLOB(I)
            VTOTAL(1,K)= VTOTAL(1,K) + V(1,I)
            VTOTAL(2,K)= VTOTAL(2,K) + V(2,I)
            VTOTAL(3,K)= VTOTAL(3,K) + V(3,I)
          ENDDO
          DO I=2,NSPMD

C   Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF 

            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                    SPMD_COMM_WORLD,STATUS,ierror)
              CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)

            CALL MPI_RECV(NGLOBI,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)
         
            MSGTYP = MSGOFF2
            CALL MPI_RECV(VRECV,3*SIZ,REAL,IT_SPMD(I),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)
 
            DO J = 1, SIZ
              K = NGLOBI(J)
              VTOTAL(1,K)= VTOTAL(1,K) + VRECV(1,J)
              VTOTAL(2,K)= VTOTAL(2,K) + VRECV(2,J)
              VTOTAL(3,K)= VTOTAL(3,K) + VRECV(3,J)
            ENDDO
          ENDDO
          CALL WRITE_R_C(VTOTAL,3*NUMNODG)

       ENDIF

       DEALLOCATE(NGLOBI)
       DEALLOCATE(VRECV)
       
#endif
       RETURN
       END
