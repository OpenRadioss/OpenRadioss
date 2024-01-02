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
Chd|  SPMD_IALLGATHERV_INT          source/mpi/generic/spmd_iallgatherv_int.F
Chd|-- called by -----------
Chd|        SPMD_TRI7VOX_OPTIMIZED        source/mpi/interfaces/spmd_tri7vox_optimized.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPMD_IALLGATHERV_INT(SENDBUF,RECVBUF,SEND_SIZE,TOTAL_RCV_SIZE,RCV_SIZE, 
     .                              DISPLS,REQUEST,COMM,size_)
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
        INTEGER, INTENT(in) :: SEND_SIZE,TOTAL_RCV_SIZE
        INTEGER, INTENT(in) :: REQUEST,COMM,size_
        INTEGER, DIMENSION(size_) :: RCV_SIZE,DISPLS
        INTEGER, DIMENSION(SEND_SIZE), INTENT(in) :: SENDBUF
        INTEGER, DIMENSION(TOTAL_RCV_SIZE), INTENT(out) :: RECVBUF

#ifdef MPI
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: error
!       ----------------------------------------

!       ------------------------------------------
!       processor P(1-->n) :
!       send buffer 
!                   |  s(m)   |
!       size =         N(m)      
!
!       For all processor :
!       rcv buffer
!       from proc :   P(1)        P(2)              P(n)
!                   |  s(1)   |   s(2)   |  ...  |  s(n)    |
!       size =          N(1)     N(2)                 N(n)
!       ------------------------------------------

!   PLMPI uses MPI-2.x version : MPI_IALLGATHERV was introduced in MPI-3.0 version
#if _PLMPI
!       ------------------
        CALL MPI_ALLGATHERV(SENDBUF,SEND_SIZE,MPI_INTEGER,
     .                   RECVBUF,RCV_SIZE,DISPLS,MPI_INTEGER,
     .                   COMM,error)
!       ------------------
#else
!       ------------------
        CALL MPI_IALLGATHERV(SENDBUF,SEND_SIZE,MPI_INTEGER,
     .                   RECVBUF,RCV_SIZE,DISPLS,MPI_INTEGER,
     .                   COMM,REQUEST,error)
!       ------------------
#endif
#else
        RECVBUF = 0

#endif
        RETURN
        END SUBROUTINE SPMD_IALLGATHERV_INT


!       ----------------------------------------
