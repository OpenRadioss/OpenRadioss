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
Chd|  SPMD_IGET_PARTN               source/mpi/anim/spmd_iget_partn.F
Chd|-- called by -----------
Chd|        ANIOFF0                       source/output/anim/generate/anioff0.F
Chd|        ANIOFFC                       source/output/anim/generate/anioffc.F
Chd|        ANIOFFC_CRK                   source/output/anim/generate/anioffc_crk.F
Chd|        ANIOFFC_PLY                   source/output/anim/generate/anioffc_ply.F
Chd|        ANIOFFF                       source/output/anim/generate/aniofff.F
Chd|        ANIOFFS                       source/output/anim/generate/anioff6.F
Chd|        DELNUMB0                      source/output/anim/generate/delnumb0.F
Chd|        DELNUMBC                      source/output/anim/generate/delnumbc.F
Chd|        DELNUMBC_CRK                  source/output/anim/generate/delnumbc_crk.F
Chd|        DELNUMBF                      source/output/anim/generate/delnumbf.F
Chd|        DELNUMBS                      source/output/anim/generate/delnumb6.F
Chd|        PARSOR0                       source/output/anim/generate/parsor0.F
Chd|        PARSORC                       source/output/anim/generate/parsorc.F
Chd|        PARSORF                       source/output/anim/generate/parsorf.F
Chd|        PARSORS                       source/output/anim/generate/parsors.F
Chd|        PARSOR_CRK                    source/output/anim/generate/parsor_crk.F
Chd|-- calls ---------------
Chd|        WRITE_C_C                     source/output/tools/sortie_c.c
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE SPMD_IGET_PARTN(SIZE,NBF_L,NP,NBPART,IADG,SRBUF,IFLAG)
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
      INTEGER  NBPART, IFLAG,SRBUF
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGTYP,INFO,IDEB,K,N,NB_TMP,LEN,IADP(NSPMD),I

      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      INTEGER, DIMENSION(:),ALLOCATABLE :: RBUF,NPT
      DATA MSGOFF/7029/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE(RBUF(SRBUF+1))
      ALLOCATE(NPT(NBF_L))

      IF (ISPMD/=0) THEN
        MSGTYP= MSGOFF

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
        MSGTYP= MSGOFF

         CALL MPI_PROBE(IT_SPMD(K),MSGTYP,
     .                   MPI_COMM_WORLD,STATUS,ierror)
         CALL MPI_GET_COUNT(STATUS,MPI_INTEGER,NB_TMP,ierror)

         CALL MPI_RECV(RBUF(IDEB),NB_TMP,MPI_INTEGER,IT_SPMD(K),MSGTYP,
     .                  MPI_COMM_WORLD,STATUS,ierror)

        IDEB = IDEB + NB_TMP
        END DO
C
        DO N = 1, NBPART
          DO K = 1, NSPMD
            IF (N>1) THEN
              LEN = (IADG(K,N) - IADG(K,N-1))*SIZE
            ELSE
              LEN = IADG(K,N)*SIZE
            ENDIF

            IF (IFLAG==1) THEN
              IF(LEN>0)CALL WRITE_I_C(RBUF(IADP(K)),LEN)
            ELSEIF(IFLAG==2) THEN
              IF(LEN>0)CALL WRITE_C_C(RBUF(IADP(K)),LEN)
            ENDIF
            IADP(K) = IADP(K) + LEN
          ENDDO
        ENDDO
      ENDIF
C
      DEALLOCATE(RBUF)
      DEALLOCATE(NPT)
#endif
      RETURN
      END
