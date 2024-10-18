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
      !||    reorder_mod           ../starter/share/modules1/reorder_mod.F
      !||--- called by ------------------------------------------------------
      !||    applysort2fvm         ../starter/source/airbag/fvmesh0.F
      !||    brokmann_elem_renum   ../starter/source/materials/fail/windshield_alter/brokmann_elem_spmd_renum.F90
      !||    c3grhead              ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails             ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c_idglob              ../starter/source/restart/ddsplit/c_idglob.F
      !||    cgrhead               ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails              ../starter/source/elements/shell/coque/cgrtails.F
      !||    dometis               ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    fractal_elem_renum    ../starter/source/materials/fail/fractal/fractal_elem_spmd_renum.F90
      !||    fvmesh0               ../starter/source/airbag/fvmesh0.F
      !||    fvmesh1               ../starter/source/airbag/fvmesh.F
      !||    init_permutation      ../starter/source/starter/lectur.F
      !||    lectur                ../starter/source/starter/lectur.F
      !||    pgrtails              ../starter/source/elements/beam/pgrtails.F
      !||    sgrhead               ../starter/source/elements/solid/solide/sgrhead.F
      !||    sgrtails              ../starter/source/elements/solid/solide/sgrtails.F
      !||    t3grhead              ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails             ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||====================================================================
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
