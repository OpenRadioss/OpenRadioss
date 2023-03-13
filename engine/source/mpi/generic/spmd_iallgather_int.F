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
Chd|  SPMD_IALLGATHER_INT           source/mpi/generic/spmd_iallgather_int.F
Chd|-- called by -----------
Chd|        SPMD_SPHVOX                   source/mpi/sph/spmd_sphvox.F  
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE SPMD_IALLGATHER_INT(SENDBUF,RECVBUF,SEND_SIZE,TOTAL_RCV_SIZE,RCV_SIZE, 
     .                              REQUEST,COMM)
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
        INTEGER, INTENT(in) :: REQUEST,COMM
        INTEGER, INTENT(in) :: RCV_SIZE
        my_real, DIMENSION(SEND_SIZE), INTENT(in) :: SENDBUF
        my_real, DIMENSION(TOTAL_RCV_SIZE), INTENT(out) :: RECVBUF

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
!       size =           N      
!
!       For all processor :
!       rcv buffer
!       from proc :   P(1)        P(2)              P(n)
!                   |  s(1)   |   s(2)   |  ...  |  s(n)    |
!       size =          N       N                    N
!       ------------------------------------------

!   PLMPI uses MPI-2.x version : MPI_IALLGATHER was introduced in MPI-3.0 version
#if _PLMPI
!       ------------------
        CALL MPI_ALLGATHER(SENDBUF,SEND_SIZE,MPI_INTEGER,
     .                   RECVBUF,RCV_SIZE,MPI_INTEGER,
     .                   COMM,error)
!       ------------------
#else
!       ------------------
        CALL MPI_IALLGATHER(SENDBUF,SEND_SIZE,MPI_INTEGER,
     .                   RECVBUF,RCV_SIZE,MPI_INTEGER,
     .                   COMM,REQUEST,error)
!       ------------------
#endif
#else
        RECVBUF = 0
#endif
        RETURN
        END SUBROUTINE SPMD_IALLGATHER_INT


!       ----------------------------------------
