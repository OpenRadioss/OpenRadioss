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
      !||====================================================================
      !||    mid_pid_mod          ../starter/share/modules1/mid_pid_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit              ../starter/source/restart/ddsplit/ddsplit.F
      !||    dometis              ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    init_mid_pid_array   ../starter/source/spmd/tools/init_mid_pid_array.F
      !||    initwg               ../starter/source/spmd/domain_decomposition/initwg.F
      !||    initwg_shell         ../starter/source/spmd/domain_decomposition/initwg_shell.F
      !||    initwg_solid         ../starter/source/spmd/domain_decomposition/initwg_solid.F
      !||    initwg_tri           ../starter/source/spmd/domain_decomposition/initwg_tri.F
      !||    lectur               ../starter/source/starter/lectur.F
      !||    w_tabmat_prop        ../starter/source/restart/ddsplit/w_tabmat_prop.F
      !||====================================================================
      MODULE MID_PID_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE MID_PID_TYPE
!--------------------------------------------------------------------
!       MID_PID_SHELL(MID)%PID1D( : )  --> (mid ; pid) pair
!       MID_PID_SHELL(MID)%NBR_PID     --> number of pid per mid
!       MID_PID_SHELL(MID)%COST1D( : ) --> element cost for each (mid ; pid)
!--------------------------------------------------------------------
        INTEGER, DIMENSION(:), ALLOCATABLE :: PID1D
        my_real, DIMENSION(:), ALLOCATABLE :: COST1D      
        INTEGER :: NBR_PID     
      END TYPE MID_PID_TYPE
C
      END MODULE MID_PID_MOD
