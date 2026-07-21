Copyright>        OpenRadioss
Copyright>        Copyright (C) 2026 Siemens
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
Copyright>        Commercial Alternative: Simcenter Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
Copyright>        commercial version may interest you: 
Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    spmd_mstop            ../engine/source/mpi/init/spmd_mstop.F
!||--- called by ------------------------------------------------------
!||    imp_sol_init          ../engine/source/implicit/imp_sol_init.F
!||--- calls      -----------------------------------------------------
!||    my_exit               ../engine/source/system/my_exit.c
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE SPMD_MSTOP()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
        USE SPMD_PROFILER_MOD, ONLY : SPMD_PROFILING_ENABLED,
     .                                SPMD_PROFILER_FLUSH
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
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER ierror
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      call MPI_BARRIER(SPMD_COMM_WORLD,ierror)
      IF (SPMD_PROFILING_ENABLED) CALL SPMD_PROFILER_FLUSH()
      CALL MPI_FINALIZE(ierror)
      IF(ISPMD/=0)THEN
        CALL MY_EXIT(0)
      ENDIF
#endif
      RETURN
      END

