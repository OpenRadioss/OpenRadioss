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
!||    spmd_split_comm       ../engine/source/mpi/init/spmd_split_comm.F
!||--- called by ------------------------------------------------------
!||    mpp_init              ../engine/source/mpi/interfaces/spmd_i7tool.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_SPLIT_COMM(ISACTIVE, COMM_LOCAL)
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISACTIVE, COMM_LOCAL
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER IVAL, IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      IVAL=ISACTIVE
      IF(ISACTIVE > 0) THEN
        IVAL = 1
      ELSE
        IVAL = MPI_UNDEFINED ! if not concerned by this communication set to undefined communicator
      END IF  
      CALL MPI_Comm_split(SPMD_COMM_WORLD, IVAL, ispmd+1, COMM_LOCAL, IERROR)
C     IF(IVAL /=0)  CALL MPI_Comm_Free(contcomm, ierror) ! only where new communicator is created (ie not MPI_UNDEFINED)

#endif
      RETURN
      END
