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
#ifdef MPI

!||====================================================================
!||    spmd_kill             ../engine/source/mpi/init/spmd_kill.F
!||--- called by ------------------------------------------------------
!||    arret                 ../engine/source/system/arret.F
!||--- calls      -----------------------------------------------------
!||    my_exit               ../engine/source/system/my_exit.c
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_KILL(ICODE)
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
      INTEGER ICODE
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER INFO,LOC_PROC,I,IERROR
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL MPI_ABORT(SPMD_COMM_WORLD,ICODE,IERROR)
      CALL MY_EXIT(ICODE)

      RETURN
      END

#elif 1 
C
!||====================================================================
!||    spmd_kill             ../engine/source/mpi/init/spmd_kill.F
!||--- called by ------------------------------------------------------
!||    arret                 ../engine/source/system/arret.F
!||--- calls      -----------------------------------------------------
!||    my_exit               ../engine/source/system/my_exit.c
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_KILL(ICODE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICODE
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      CALL MY_EXIT(ICODE)
      END


#endif


