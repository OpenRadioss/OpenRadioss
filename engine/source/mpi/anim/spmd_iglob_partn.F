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
      !||    spmd_iglob_partn   ../engine/source/mpi/anim/spmd_iglob_partn.F
      !||--- called by ------------------------------------------------------
      !||    parsor0            ../engine/source/output/anim/generate/parsor0.F
      !||    parsor_crk         ../engine/source/output/anim/generate/parsor_crk.F
      !||    parsor_ply         ../engine/source/output/anim/generate/parsor_ply.F
      !||    parsorc            ../engine/source/output/anim/generate/parsorc.F
      !||    parsorf            ../engine/source/output/anim/generate/parsorf.F
      !||    parsors            ../engine/source/output/anim/generate/parsors.F
      !||--- calls      -----------------------------------------------------
      !||====================================================================
      SUBROUTINE SPMD_IGLOB_PARTN(IAD,NBPART,IADG,SBUF)
C iglob_part reconstruit sur p0 un tableau global de part 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
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
      INTEGER IAD(*), NBPART, IADG(NSPMD,NBPART)
      INTEGER SBUF
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,K,N,NB_TMP
      INTEGER, DIMENSION(:),ALLOCATABLE :: RECBUF
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR

      DATA MSGOFF/7028/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(RECBUF(SBUF))

      IF (ISPMD/=0) THEN
         MSGTYP= MSGOFF
         CALL MPI_SEND(IAD,NBPART,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     .                SPMD_COMM_WORLD,ierror)

      ELSE
        DO N = 1, NBPART
          IADG(1,N) = IAD(N)
        ENDDO

        DO K=2,NSPMD
          MSGTYP= MSGOFF
          CALL MPI_PROBE(IT_SPMD(K),MSGTYP,
     .                   SPMD_COMM_WORLD,STATUS,ierror)
          CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,NB_TMP,ierror)
          CALL MPI_RECV(RECBUF,NB_TMP,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  SPMD_COMM_WORLD,STATUS,ierror)

          DO N = 1, NBPART
            IADG(K,N) = RECBUF(N)
          ENDDO
        END DO
      ENDIF

      DEALLOCATE(RECBUF)

#endif
      RETURN
      END
