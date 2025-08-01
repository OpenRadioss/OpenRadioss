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
!||    spmd_gatherv          ../engine/source/mpi/generic/spmd_gatherv.F
!||--- called by ------------------------------------------------------
!||    hist2                 ../engine/source/output/th/hist2.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
        SUBROUTINE SPMD_GATHERV(SENDBUF,RECVBUF,PROC,SEND_SIZE,TOTAL_RCV_SIZE,RCV_SIZE,DIPLS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------------------------
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
        INTEGER, INTENT(in) :: SEND_SIZE,TOTAL_RCV_SIZE
        INTEGER, INTENT(in) :: PROC
        INTEGER, DIMENSION(NSPMD) :: RCV_SIZE,DIPLS
        my_real, DIMENSION(SEND_SIZE), INTENT(in) :: SENDBUF
        my_real, DIMENSION(TOTAL_RCV_SIZE), INTENT(out) :: RECVBUF

#ifdef MPI
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
        INTEGER :: error
!       ----------------------------------------

!       ------------------
!       mpi comm gahterv on processor PROC
        CALL MPI_GATHERV(SENDBUF,SEND_SIZE,REAL,
     .                   RECVBUF,RCV_SIZE,DIPLS,REAL,
     .                   PROC,SPMD_COMM_WORLD,error)
!       ------------------

#else
        RECVBUF = 0
#endif
        RETURN
        END SUBROUTINE SPMD_GATHERV
!       ----------------------------------------
