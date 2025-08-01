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
!||    spmd_wrt_crk_xyznod   ../engine/source/mpi/anim/spmd_wrt_crk_xyznod.F
!||--- called by ------------------------------------------------------
!||    xyznod_crk            ../engine/source/output/anim/generate/xyznod_crk.F
!||--- calls      -----------------------------------------------------
!||    write_r_c             ../common_source/tools/input_output/write_routtines.c
!||--- uses       -----------------------------------------------------
!||    crackxfem_mod         ../engine/share/modules/crackxfem_mod.F
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
       SUBROUTINE SPMD_WRT_CRK_XYZNOD(ICRK,NUM,NODGLOBXFE)
C-----------------------------------------------
      USE CRACKXFEM_MOD        
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
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER ICRK,NUM,NODGLOBXFE(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF,EMPL
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,MSGOFF2,MSGTYP2,SIZ0,STEP
C
      REAL, DIMENSION(:,:), ALLOCATABLE :: BUFSR,XGLOB
      INTEGER, DIMENSION(:), ALLOCATABLE ::  IBUF
C-----------------------------------------------
      DATA MSGOFF/7064/
      DATA MSGOFF2/7065/
C-----------------------------------------------
       ALLOCATE(BUFSR(3,NCRKNODG))
       ALLOCATE(IBUF(NCRKNODG))
       ALLOCATE(XGLOB(3,NUM))
C-----------------------------------------------

       IF (ISPMD/=0) THEN
         SIZ = 0
         STEP = 0
         DO I=1,CRKNOD(ICRK)%CRKNUMNODS
           STEP = CRKNOD(ICRK)%CRKNUMNODS*(ICRK-1)
           SIZ = SIZ + 1
           IBUF(SIZ) = NODGLOBXFE(I+STEP)
           BUFSR(1,SIZ) = CRKAVX(ICRK)%XX(1,I)
           BUFSR(2,SIZ) = CRKAVX(ICRK)%XX(2,I)
           BUFSR(3,SIZ) = CRKAVX(ICRK)%XX(3,I)
         END DO
C
C   a cause de la version simple precision, on ne peux pas metre l'entier
C   dans le buffer flottant car on n a que 2puiss 24 bits dispo ~ 16 Million
C   de noeuds au max
C
         MSGTYP = MSGOFF2 
         CALL MPI_SEND(IBUF,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                 SPMD_COMM_WORLD,ierror)
C  
         MSGTYP = MSGOFF
         CALL MPI_SEND(BUFSR,3*SIZ,MPI_REAL4,IT_SPMD(1),MSGTYP,
     .                 SPMD_COMM_WORLD,ierror)
       ELSE
           STEP = 0
C
C proc 0
C
          DO I=1,CRKNOD(ICRK)%CRKNUMNODS
            STEP = CRKNOD(ICRK)%CRKNUMNODS*(ICRK-1)
            NG = NODGLOBXFE(I+STEP)
            XGLOB(1,NG) = CRKAVX(ICRK)%XX(1,I)
            XGLOB(2,NG) = CRKAVX(ICRK)%XX(2,I)
            XGLOB(3,NG) = CRKAVX(ICRK)%XX(3,I)
cc            CALL WRITE_R_C(XGLOB(1,NG),1)
cc            CALL WRITE_R_C(XGLOB(2,NG),1)
cc            CALL WRITE_R_C(XGLOB(3,NG),1)
          ENDDO
C------------
          DO I=2,NSPMD

C   Reception du buffer entier des adresses NODGLOB
            MSGTYP = MSGOFF2
            CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                     SPMD_COMM_WORLD,STATUS,ierror)
              CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)
            CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                    SPMD_COMM_WORLD,STATUS,ierror)
c
C   Reception du buffer flottant des adresses NODGLOB
c
            MSGTYP2 = MSGOFF
c
            CALL MPI_RECV(BUFSR,SIZ*3,MPI_REAL4,IT_SPMD(I),MSGTYP2,
     .                    SPMD_COMM_WORLD,STATUS,ierror)
c
            NREC = SIZ 
c
            DO K = 1, NREC
              NG = IBUF(K)
              XGLOB(1,NG) = BUFSR(1,K)
              XGLOB(2,NG) = BUFSR(2,K)
              XGLOB(3,NG) = BUFSR(3,K)
cc              CALL WRITE_R_C(XGLOB(1,NG),1)
cc              CALL WRITE_R_C(XGLOB(2,NG),1)
cc              CALL WRITE_R_C(XGLOB(3,NG),1)
            ENDDO
          ENDDO
C
c          DO I=1,NUM
cc          DO I=1,NCRKNODG
cc           CALL WRITE_R_C(XGLOB(1,I),1)
cc           CALL WRITE_R_C(XGLOB(2,I),1)
cc           CALL WRITE_R_C(XGLOB(3,I),1)
cc         END DO
C
        DO I=1,NCRKNODG
          K=I+NCRKNODG*(ICRK-1)
          CALL WRITE_R_C(XGLOB(1,K),1)
          CALL WRITE_R_C(XGLOB(2,K),1)
          CALL WRITE_R_C(XGLOB(3,K),1)
        ENDDO
C
        END IF
C---
C-----------------------------------------------
       DEALLOCATE(BUFSR)
       DEALLOCATE(IBUF)
       DEALLOCATE(XGLOB)
C-----------------------------------------------
#endif
       RETURN
       END
