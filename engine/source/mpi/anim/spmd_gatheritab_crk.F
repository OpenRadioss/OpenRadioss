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
Chd|  SPMD_GATHERITAB_CRK           source/mpi/anim/spmd_gatheritab_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
       SUBROUTINE SPMD_GATHERITAB_CRK(ICRK,NUM,IDMAXNOD,NODGLOBXFE)
C-----------------------------------------------
      USE CRACKXFEM_MOD        
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER ICRK,NUM,IDMAXNOD,NODGLOBXFE(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER SIZ,MSGTYP,I,K,NG,NREC,STEP

C   Tableau utilise par proc 0
       INTEGER, DIMENSION(:,:),ALLOCATABLE :: IBUF
       INTEGER, DIMENSION(:),ALLOCATABLE :: XGLOB
C-----------------------------------------------
      DATA MSGOFF/7068/
C-----------------------------------------------
       ALLOCATE(IBUF(2,NCRKNODG))
       ALLOCATE(XGLOB(NUM))

       IF(ISPMD /= 0)THEN
         SIZ = 0
         STEP = 0
         DO I=1,CRKNOD(ICRK)%CRKNUMNODS
           SIZ = SIZ + 1
           STEP = CRKNOD(ICRK)%CRKNUMNODS*(ICRK-1)
           IBUF(1,SIZ) = NODGLOBXFE(I+STEP)
           IBUF(2,SIZ) = CRKNOD(ICRK)%XFECRKNODID(I)+IDMAXNOD
         END DO
C
         MSGTYP = MSGOFF
         CALL MPI_SEND(IBUF,2*SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                 MPI_COMM_WORLD,ierror)
       ELSE
C proc 0 
       STEP = 0
        DO I=1,CRKNOD(ICRK)%CRKNUMNODS
          STEP = CRKNOD(ICRK)%CRKNUMNODS*(ICRK-1)
          NG = NODGLOBXFE(I+STEP)
          XGLOB(NG) = CRKNOD(ICRK)%XFECRKNODID(I)+IDMAXNOD
cc          CALL WRITE_I_C(XGLOB(NG),1)
        ENDDO
C all the rest procs
        DO I=2,NSPMD
C   Reception du buffer entier des adresses NODGLOB
          MSGTYP = MSGOFF 
          CALL MPI_PROBE(IT_SPMD(I),MSGTYP,
     .                   MPI_COMM_WORLD,STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,SIZ,ierror)
          CALL MPI_RECV(IBUF,SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)
C         
          NREC = SIZ/2
          DO K = 1, NREC
            NG = IBUF(1,K)
            XGLOB(NG) = IBUF(2,K)
cc            CALL WRITE_I_C(XGLOB(NG),1)
          ENDDO
        ENDDO ! DO I=2,NSPMD
cc          CALL WRITE_I_C(XGLOB,NUM)
C
        DO I=1,NCRKNODG
          K=I+NCRKNODG*(ICRK-1)
          CALL WRITE_I_C(XGLOB(K),1)
        ENDDO
C
       ENDIF
C
       DEALLOCATE(IBUF)
       DEALLOCATE(XGLOB)
#endif
       RETURN
       END
