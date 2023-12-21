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
Chd|  ALEANIM_MOD                   share/modules/aleanim_mod.F   
Chd|-- called by -----------
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|        INIRESA                       source/output/restart/rdresa.F
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        SDEFO3                        source/elements/solid/solide/sdefo3.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ALEANIM_MOD
      
#include "my_real.inc"

        TYPE FANI_CELL_
          LOGICAL IS_F18_FVM_REQUESTED
          LOGICAL IS_VORT_X_REQUESTED
          LOGICAL IS_VORT_Y_REQUESTED
          LOGICAL IS_VORT_Z_REQUESTED
          my_real,DIMENSION(:,:), ALLOCATABLE :: F18
          my_real,DIMENSION(:), ALLOCATABLE :: VORT_X
          my_real,DIMENSION(:), ALLOCATABLE :: VORT_Y
          my_real,DIMENSION(:), ALLOCATABLE :: VORT_Z
        END TYPE

        TYPE(FANI_CELL_) :: FANI_CELL


      END MODULE ALEANIM_MOD
