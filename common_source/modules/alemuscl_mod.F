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
Chd|  ALEMUSCL_MOD                  modules/alemuscl_mod.F        
Chd|-- called by -----------
Chd|        ALE51_GRADIENT_RECONSTRUCTION engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALEMUSCL_DEALLOCATE           engine/source/ale/alemuscl/alemuscl_deallocate.F
Chd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
Chd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
Chd|        GRADIENT_LIMITATION           engine/source/ale/alemuscl/gradient_limitation.F
Chd|        GRADIENT_LIMITATION2          engine/source/ale/alemuscl/gradient_limitation2.F
Chd|        GRADIENT_RECONSTRUCTION       engine/source/ale/alemuscl/gradient_reconstruction.F
Chd|        GRADIENT_RECONSTRUCTION2      engine/source/ale/alemuscl/gradient_reconstruction2.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ALEMUSCL_MOD

#include      "my_real.inc"
     
      my_real, DIMENSION(:, :), ALLOCATABLE :: VOLUME_FRACTION
      my_real, DIMENSION(:, :), ALLOCATABLE :: NODE_MAX_VALUE, NODE_MIN_VALUE
      my_real, DIMENSION(:, :, :), ALLOCATABLE :: GRAD
      my_real, DIMENSION(:, :), ALLOCATABLE :: ELCENTER
      INTEGER, POINTER, DIMENSION(:) :: pCNEL, pADDCNEL, pADDTMPL
      
      END MODULE ALEMUSCL_MOD
