Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  SPMD_R4GET_PARTN              source/mpi/anim/spmd_r4get_partn.F
Chd|-- called by -----------
Chd|        ANIMCALE                      source/output/anim/generate/genani.F
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCC_CRK                    source/output/anim/generate/dfuncc_crk.F
Chd|        DFUNCC_PLY                    source/output/anim/generate/dfuncc_ply.F
Chd|        DFUNCF                        source/output/anim/generate/dfuncf.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        TENSOR0                       source/output/anim/generate/tensor0.F
Chd|        TENSORC                       source/output/anim/generate/tensorc.F
Chd|        TENSORC_CRK                   source/output/anim/generate/tensorc_crk.F
Chd|        TENSORC_PLY                   source/output/anim/generate/tensorc_ply.F
Chd|        TENSORS                       source/output/anim/generate/tensor6.F
Chd|        TORSEUR                       source/output/anim/generate/torseur.F
Chd|-- calls ---------------
Chd|        WRITE_R_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE SPMD_R4GET_PARTN(SIZE,NBF_L,NBPART,IADG,WAL,BUF)
C gather sur p0 du tableau wa en fonction des parts (IADG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif

C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBF_L, NBPART, SIZE, IFLAG, IADG(NSPMD,*),BUF
      REAL WAL(*)
      REAL, DIMENSION(:), ALLOCATABLE :: WAR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,IDEB,K,N,NB_TMP,LEN,IADP(NSPMD)
      REAL AUX
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DATA MSGOFF/7002/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(WAR(BUF+1))

      IF (ISPMD/=0) THEN
        MSGTYP=MSGOFF
        CALL MPI_SEND(WAL,NBF_L,MPI_REAL4,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,ierror)

      ELSE
C  
        DO K=1,NBF_L
          WAR(K) = WAL(K)
        END DO

        IDEB = NBF_L + 1
        IADP(1) = 1
C
        DO K=2,NSPMD
          IADP(K) = IDEB
          MSGTYP=MSGOFF

          CALL MPI_PROBE(IT_SPMD(K),MSGTYP,
     .                   MPI_COMM_WORLD,STATUS,ierror)
          CALL MPI_GET_COUNT(STATUS,MPI_REAL4,NB_TMP,ierror)
          CALL MPI_RECV(WAR(IDEB),NB_TMP,MPI_REAL4,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

          IDEB = IDEB + NB_TMP
        END DO
        DO N = 1, NBPART
          DO K = 1, NSPMD
            IF (N>1) THEN
              LEN = (IADG(K,N) - IADG(K,N-1))*SIZE
            ELSE
              LEN = IADG(K,N)*SIZE
            ENDIF
            IF(LEN>0)CALL WRITE_R_C(WAR(IADP(K)),LEN)
            IADP(K) = IADP(K) + LEN
          ENDDO
        ENDDO
      ENDIF
C
      DEALLOCATE(WAR)
#endif
      RETURN
      END
