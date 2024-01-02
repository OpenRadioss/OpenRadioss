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
Chd|  DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|-- called by -----------
Chd|        INITWG                        source/spmd/domain_decomposition/initwg.F
Chd|        INITWG_POUTRE                 source/spmd/domain_decomposition/initwg_poutre.F
Chd|        INITWG_QUAD                   source/spmd/domain_decomposition/initwg_quad.F
Chd|        INITWG_RESSORT                source/spmd/domain_decomposition/initwg_ressort.F
Chd|        INITWG_SHELL                  source/spmd/domain_decomposition/initwg_shell.F
Chd|        INITWG_SOLID                  source/spmd/domain_decomposition/initwg_solid.F
Chd|        INITWG_TRI                    source/spmd/domain_decomposition/initwg_tri.F
Chd|        INITWG_TRUSS                  source/spmd/domain_decomposition/initwg_truss.F
Chd|        INITWG_X                      source/spmd/domain_decomposition/initwg_x.F
Chd|        INIT_MAT_WEIGHT               source/user_interface/set_dd_mat_weight.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        R2R_VOID                      source/coupling/rad2rad/r2r_void.F
Chd|        SET_DD_MAT_WEIGHT             source/user_interface/set_dd_mat_weight.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE DDWEIGHTS_MOD
C-----------------------------------------------
C   I n c l u d e s 
C-----------------------------------------------
#include "my_real.inc"
         my_real , DIMENSION(:,:,:), ALLOCATABLE :: DDWEIGHTS
         my_real MAT_TIME_REF
C-----------------------------------------------
      END MODULE DDWEIGHTS_MOD

