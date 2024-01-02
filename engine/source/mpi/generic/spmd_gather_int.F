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
Chd|  SPMD_GATHER_INT               source/mpi/generic/spmd_gather_int.F
Chd|-- called by -----------
Chd|        INIT_TH                       source/output/th/init_th.F    
Chd|        READ_DYNAIN                   source/output/dynain/read_dynain.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPMD_GATHER_INT(SENDBUF,RECVBUF,PROC,SEND_SIZE,RCV_SIZE)
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: SEND_SIZE
        INTEGER, INTENT(in) :: PROC
        INTEGER, INTENT(in) :: RCV_SIZE
        INTEGER, DIMENSION(SEND_SIZE), INTENT(in) :: SENDBUF
        INTEGER, DIMENSION(RCV_SIZE), INTENT(out) :: RECVBUF

#ifdef MPI
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: error
!       ----------------------------------------

!       ------------------
!       mpi comm gahter on processor PROC
        CALL MPI_GATHER(SENDBUF,SEND_SIZE,MPI_INTEGER,
     .                  RECVBUF,SEND_SIZE,MPI_INTEGER,
     .                  PROC,MPI_COMM_WORLD,error)
!       ------------------

#else
        RECVBUF = 0
#endif
        RETURN
        END SUBROUTINE SPMD_GATHER_INT
!       ----------------------------------------
