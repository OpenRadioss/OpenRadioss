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
Chd|  SPMD_GATH_COLLECTIVE          source/mpi/generic/spmd_gath_collective.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GATH_COLLECTIVE(SRBUF,LEN,LRECV)
C Description: 
C Collective communication version of SPMD_IGATH 
C expected to be less efficient than SPMD_IGATH 
C But useful to debug MPI problems: less pending communication 
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
      INTEGER SIZES(NSPMD),POINTERS(NSPMD)
      INTEGER,ALLOCATABLE,DIMENSION(:) :: SRBUF2
      DATA MSGOFF/7013/
      DATA MSGOFF2/7068/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------

      S1 = 1

      ! Send Local sizes to Zero
      CALL MPI_GATHER(LEN,S1,MPI_INTEGER,SIZES,S1,MPI_INTEGER,0,MPI_COMM_WORLD,ierror)


      IF(ISPMD == 0) THEN
        LRECV = 0 
        DO I=1,NSPMD
          LRECV = LRECV + SIZES(I)
        ENDDO
        ! Pointers starts at 0
        POINTERS(1) = 0
        DO I=2,NSPMD
          POINTERS(I) = POINTERS(I-1)+SIZES(I-1)
        ENDDO 
        ALLOCATE(SRBUF2(LRECV))
      ELSE
        ALLOCATE(SRBUF2(1))
      ENDIF

      CALL MPI_GATHERV(SRBUF,LEN,MPI_INTEGER,SRBUF2,SIZES,POINTERS,MPI_INTEGER,0,MPI_COMM_WORLD,ierror)


      CALL MPI_BARRIER(MPI_COMM_WORLD,ierror)

      IF(ISPMD == 0) THEN
        DO I=1,LRECV
          SRBUF(I) = SRBUF2(I)  
        ENDDO 
      ENDIF
      DEALLOCATE(SRBUF2)

C
#endif
      RETURN
      END
