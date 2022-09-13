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
Chd|  SPMD_FVB_AMAX                 source/mpi/anim/spmd_fvb_amax.F
Chd|-- called by -----------
Chd|        DELNUMBC                      source/output/anim/generate/delnumbc.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_FVB_AMAX(IDCMAX, IDCMAXL)
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
      INTEGER IDCMAX, IDCMAXL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER I, ITAG, MSGOFF, IDCMAXP(NSPMD-1), REQ(NSPMD-1),
     .        STAT(MPI_STATUS_SIZE,NSPMD-1), IERR
C-----------------------------------------------
      DATA MSGOFF/7053/
C-----------------------------------------------
C
      IF (ISPMD==0) THEN
         DO I=1,NSPMD-1
            ITAG=MSGOFF
            CALL MPI_IRECV(IDCMAXP(I), 1, MPI_INTEGER, IT_SPMD(I+1),
     .                     ITAG, MPI_COMM_WORLD, REQ(I), IERR)
         ENDDO
         CALL MPI_WAITALL(NSPMD-1, REQ, STAT, IERR)
C         
         IDCMAX=IDCMAXL
         DO I=1,NSPMD-1
            IDCMAX=MAX(IDCMAX,IDCMAXP(I))
         ENDDO
      ELSE
         ITAG=MSGOFF
         CALL MPI_ISEND(IDCMAXL, 1, MPI_INTEGER, IT_SPMD(1),
     .                  ITAG, MPI_COMM_WORLD, REQ, IERR)
         CALL MPI_WAIT(REQ, STAT, IERR)
      ENDIF
C
#endif
      RETURN
      END
