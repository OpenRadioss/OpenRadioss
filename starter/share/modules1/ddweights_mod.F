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
      !||    ddweights_mod       ../starter/share/modules1/ddweights_mod.F
      !||--- called by ------------------------------------------------------
      !||    init_mat_weight     ../starter/source/user_interface/set_dd_mat_weight.F
      !||    initwg              ../starter/source/spmd/domain_decomposition/initwg.F
      !||    initwg_poutre       ../starter/source/spmd/domain_decomposition/initwg_poutre.F
      !||    initwg_quad         ../starter/source/spmd/domain_decomposition/initwg_quad.F
      !||    initwg_ressort      ../starter/source/spmd/domain_decomposition/initwg_ressort.F
      !||    initwg_shell        ../starter/source/spmd/domain_decomposition/initwg_shell.F
      !||    initwg_solid        ../starter/source/spmd/domain_decomposition/initwg_solid.F
      !||    initwg_tri          ../starter/source/spmd/domain_decomposition/initwg_tri.F
      !||    initwg_truss        ../starter/source/spmd/domain_decomposition/initwg_truss.F
      !||    initwg_x            ../starter/source/spmd/domain_decomposition/initwg_x.F
      !||    lectur              ../starter/source/starter/lectur.F
      !||    r2r_void            ../starter/source/coupling/rad2rad/r2r_void.F
      !||    set_dd_mat_weight   ../starter/source/user_interface/set_dd_mat_weight.F
      !||====================================================================
      MODULE DDWEIGHTS_MOD
C-----------------------------------------------
C   I n c l u d e s 
C-----------------------------------------------
#include "my_real.inc"
         my_real , DIMENSION(:,:,:), ALLOCATABLE :: DDWEIGHTS
         my_real MAT_TIME_REF
C-----------------------------------------------
      END MODULE DDWEIGHTS_MOD

