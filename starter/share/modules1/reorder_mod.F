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
Chd|  REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|-- called by -----------
Chd|        APPLYSORT2FVM                 source/airbag/fvmesh0.F       
Chd|        C3GRHEAD                      source/elements/sh3n/coque3n/c3grhead.F
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        CGRHEAD                       source/elements/shell/coque/cgrhead.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        C_IDGLOB                      source/restart/ddsplit/c_idglob.F
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        FVMESH1                       source/airbag/fvmesh.F        
Chd|        INIT_PERMUTATION              source/starter/lectur.F       
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        PGRTAILS                      source/elements/beam/pgrtails.F
Chd|        SGRHEAD                       source/elements/solid/solide/sgrhead.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        T3GRHEAD                      source/elements/solid_2d/tria/t3grhead.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|-- calls ---------------
Chd|====================================================================
          MODULE REORDER_MOD
          
          TYPE REORDER_STRUCT_
            INTEGER, DIMENSION(:),ALLOCATABLE :: SOLID
            INTEGER, DIMENSION(:),ALLOCATABLE :: SHELL
            INTEGER, DIMENSION(:),ALLOCATABLE :: TRIANGLE
            INTEGER, DIMENSION(:),ALLOCATABLE :: QUAD
          END TYPE REORDER_STRUCT_
          
          TYPE(REORDER_STRUCT_) PERMUTATION 
          END MODULE REORDER_MOD
C=======================================================================        
