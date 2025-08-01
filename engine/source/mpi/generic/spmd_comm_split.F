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
!||    spmd_comm_split       ../engine/source/mpi/generic/spmd_comm_split.F
!||--- called by ------------------------------------------------------
!||    fvrrest               ../engine/source/output/restart/rdresb.F
!||    spmd_i25front_init    ../engine/source/mpi/interfaces/spmd_i25front.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_COMM_SPLIT(COLOR,KEY,SUBCOMM,RANK,SIZE_L) 
C Wrapper to MPI_COMM_SPLIT: returns RANK = -1 and
C  SUBCOMM = MPI_COMM_NULL if the processor does not belong the the
C  subcommunicator 
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER COMM        ! communicator IN
      INTEGER SUBCOMM     ! communicator OUT
      INTEGER RANK        ! rank in the new comm.
      INTEGER SIZE_L      ! size of the new comm.
      INTEGER COLOR,KEY   ! color
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL MPI_BARRIER(SPMD_COMM_WORLD,IERROR)
      CALL MPI_COMM_SPLIT(SPMD_COMM_WORLD,COLOR,KEY,SUBCOMM,IERROR)
      IF(COLOR > 0) THEN
        CALL MPI_COMM_RANK(SUBCOMM, RANK, IERROR)
        CALL MPI_COMM_SIZE(SUBCOMM, SIZE_L, IERROR)
      ELSE
        RANK = -1 
        SUBCOMM = MPI_COMM_NULL
      ENDIF
      CALL MPI_BARRIER(SPMD_COMM_WORLD,IERROR)


#endif
      RETURN
      END
