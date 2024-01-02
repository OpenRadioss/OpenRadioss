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
Chd|  SCHLIEREN_MOD                 share/modules/schlieren_mod.F 
Chd|-- called by -----------
Chd|        DFUNC0                        source/output/anim/generate/dfunc0.F
Chd|        DFUNCC                        source/output/anim/generate/dfuncc.F
Chd|        DFUNCS                        source/output/anim/generate/dfunc6.F
Chd|        H3D_QUAD_SCALAR               source/output/h3d/h3d_results/h3d_quad_scalar.F
Chd|        H3D_QUAD_VECTOR               source/output/h3d/h3d_results/h3d_quad_vector.F
Chd|        H3D_SHELL_SCALAR              source/output/h3d/h3d_results/h3d_shell_scalar.F
Chd|        H3D_SHELL_SCALAR_1            source/output/h3d/h3d_results/h3d_shell_scalar_1.F
Chd|        H3D_SHELL_VECTOR              source/output/h3d/h3d_results/h3d_shell_vector.F
Chd|        H3D_SHELL_VECTOR_1            source/output/h3d/h3d_results/h3d_shell_vector_1.F
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SOLID_TENSOR              source/output/h3d/h3d_results/h3d_solid_tensor.F
Chd|        H3D_SOLID_TENSOR_1            source/output/h3d/h3d_results/h3d_solid_tensor_1.F
Chd|        H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        H3D_SPH_SCALAR                source/output/h3d/h3d_results/h3d_sph_scalar.F
Chd|        H3D_SPH_TENSOR                source/output/h3d/h3d_results/h3d_sph_tensor.F
Chd|        SCHLIEREN_BUFFER_GATHERING    source/output/anim/generate/schlieren_buffer_gathering.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SCHLIEREN_MOD
#include "my_real.inc"

      my_real, DIMENSION(:), ALLOCATABLE :: WA_L

      END MODULE SCHLIEREN_MOD
