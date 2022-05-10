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
Chd|  SPMD_IGET_PARTN_PLY           source/mpi/anim/spmd_iget_partn_ply.F
Chd|-- called by -----------
Chd|        DELNUMBC_PLY                  source/output/anim/generate/delnumbc_ply.F
Chd|        PARSOR_PLY                    source/output/anim/generate/parsor_ply.F
Chd|-- calls ---------------
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE SPMD_IGET_PARTN_PLY(SIZE,NBF_L,NP,NBPART,IADG,
     *           SRBUF,IFLAG,IDPLY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C gather sur p0 du tableau wa en fonction des parts (IADG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "com04_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NBF_L, NP(*),IADG(NSPMD,*),SIZE
      INTEGER  NBPART, IFLAG,SRBUF,IDPLY
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,IDEB,K,N,NB_TMP,LEN,IADP(NSPMD),
     .        I,EMPL,IPLY

      INTEGER, DIMENSION(:),ALLOCATABLE :: RBUF,RBUF2,NPT
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      INTEGER  GET_PLY_NOD
      EXTERNAL GET_PLY_NOD
      DATA MSGOFF/7057/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(RBUF(SRBUF))
      ALLOCATE(RBUF2(SRBUF))
      ALLOCATE(NPT(NBF_L))

      IF (ISPMD.NE.0) THEN
        MSGTYP=MSGOFF

        CALL MPI_SEND(NP,NBF_L,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,ierror)

      ELSE
        DO K=1,NBF_L
           RBUF(K) = NP(K)
        ENDDO
        IDEB = NBF_L +  1
        IADP(1) = 1
C
      DO K=2,NSPMD
        IADP(K) = IDEB
        MSGTYP=MSGOFF

         CALL MPI_PROBE(IT_SPMD(K),MSGTYP,
     .                   MPI_COMM_WORLD,STATUS,ierror)
         CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,NB_TMP,ierror)

         CALL MPI_RECV(RBUF(IDEB),NB_TMP,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

        IDEB = IDEB + NB_TMP
        END DO
C
        DO N = 1, NBPART
          IPLY=INDX_PLY(N)
          DO K = 1, NSPMD
            IF (N.GT.1) THEN
              LEN = (IADG(K,N) - IADG(K,N-1))*SIZE
            ELSE
              LEN = IADG(K,N)*SIZE
            ENDIF
            IF (IFLAG.EQ.1) THEN
              DO I=IADP(K), IADP(K)+LEN-1
                RBUF(I) = RBUF(I)+IDPLY
              ENDDO
              CALL WRITE_I_C(RBUF(IADP(K)),LEN)
            ELSEIF(IFLAG.EQ.2) THEN
              CALL WRITE_C_C(RBUF(IADP(K)),LEN)
            ENDIF
            IADP(K) = IADP(K) + LEN
          ENDDO
        ENDDO
      ENDIF
C
      DEALLOCATE(RBUF)
      DEALLOCATE(RBUF2)
      DEALLOCATE(NPT)
#endif
      RETURN
      END
