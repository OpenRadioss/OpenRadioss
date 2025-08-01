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
!||    spmd_ialltoall   ../engine/source/mpi/generic/spmd_ialltoall.F
!||--- called by ------------------------------------------------------
!||    spmd_sphgat      ../engine/source/mpi/sph/spmd_sphgat.F
!||--- calls      -----------------------------------------------------
!||====================================================================
        SUBROUTINE SPMD_IALLTOALL(SENDBUF,RECVBUF,TOTAL_SEND_SIZE,SEND_SIZE,
     .                            TOTAL_RCV_SIZE,RCV_SIZE,REQUEST,COMM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER, INTENT(in) :: SEND_SIZE,TOTAL_SEND_SIZE
        INTEGER, INTENT(in) :: REQUEST,COMM
        INTEGER, INTENT(in) :: RCV_SIZE,TOTAL_RCV_SIZE
        my_real, DIMENSION(TOTAL_SEND_SIZE), INTENT(in) :: SENDBUF
        my_real, DIMENSION(TOTAL_RCV_SIZE), INTENT(out) :: RECVBUF

#ifdef MPI
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: error
!       ----------------------------------------

!       ------------------------------------------
!       processor P(m) :
!       send buffer 
!          to proc :   P(1)     P(2)           P(n)
!                   |  s1   |   s2   |  ...  |  sn    |
!       size =         N        N                N
!
!       rcv buffer 
!       from proc :   P(1)     P(2)           P(n)
!                   |  r1   |   r2   |  ...  |  rn    |
!       size =         N        N                N
!       ------------------------------------------

!   PLMPI uses MPI-2.x version : MPI_IALLTOALL was introduced in MPI-3.0 version
#if _PLMPI
!       mpi comm alltoall
        CALL MPI_ALLTOALL(SENDBUF,SEND_SIZE,REAL,
     .                     RECVBUF,RCV_SIZE,REAL,
     .                     COMM,error)
!       ------------------
#else
!       ------------------
!       mpi comm alltoall
        CALL MPI_IALLTOALL(SENDBUF,SEND_SIZE,REAL,
     .                     RECVBUF,RCV_SIZE,REAL,
     .                     COMM,REQUEST,error)
!       ------------------
#endif
#else
        RECVBUF = 0
#endif
        RETURN
        END SUBROUTINE SPMD_IALLTOALL


!       ----------------------------------------
