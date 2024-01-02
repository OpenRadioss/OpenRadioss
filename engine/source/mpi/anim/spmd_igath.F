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
Chd|  SPMD_IGATH                    source/mpi/anim/spmd_igath.F  
Chd|-- called by -----------
Chd|        ANISKEW                       source/output/anim/generate/aniskew.F
Chd|        ANISKEWF                      source/output/anim/generate/aniskewf.F
Chd|        DSPHNOR                       source/output/anim/generate/dsphnor.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_IGATH(SRBUF,LEN,LRECV)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
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
      INTEGER SRBUF(*), LEN, LRECV
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,MSGOFF2,MSGTYP,INFO,I,K,ATID,ATAG,ALEN,RLEN,S1
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      DATA MSGOFF/7013/
      DATA MSGOFF2/7068/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      S1 = 1
      IF (ISPMD/=0) THEN
        MSGTYP=MSGOFF2

        CALL MPI_SEND(LEN,S1,MPI_INTEGER,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,ierror)

        MSGTYP=MSGOFF
        IF(LEN > 0 ) THEN
          CALL MPI_SEND(SRBUF,LEN,MPI_INTEGER,
     .                IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,ierror)
        ENDIF

      ELSE

        LRECV = LEN

        DO I=2,NSPMD
            MSGTYP=MSGOFF2
            CALL MPI_RECV(RLEN,S1,MPI_INTEGER,
     .                    IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)
            MSGTYP=MSGOFF

            IF(RLEN > 0 ) THEN
              ! Even though Receive of length = 0 can cost nothing
              ! SRBUF(LRECV+1) must exist
              CALL MPI_RECV(SRBUF(LRECV+1),RLEN,MPI_INTEGER,
     .                    IT_SPMD(I),MSGTYP,
     .                    MPI_COMM_WORLD,STATUS,ierror)

            ENDIF
            LRECV = LRECV + RLEN 

        END DO

      ENDIF
C
#endif
      RETURN
      END
