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
      !||    table4d_mod         ../common_source/modules/table4d_mod.F
      !||--- called by ------------------------------------------------------
      !||    asso_plas76         ../engine/source/materials/mat/mat076/asso_plas76.F
      !||    asso_qplas76c       ../engine/source/materials/mat/mat076/asso_qplas76c.F
      !||    condamage           ../engine/source/materials/mat/mat190/condamage.F
      !||    conversion          ../engine/source/materials/mat/mat190/conversion.F
      !||    fail_param_mod      ../common_source/modules/mat_elem/fail_param_mod.F90
      !||    law50_table         ../starter/source/materials/mat/mat050/law50_table.F90
      !||    law70_table         ../starter/source/materials/mat/mat070/law70_table.F
      !||    matparam_def_mod    ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    no_asso_lplas76c    ../engine/source/materials/mat/mat076/no_asso_lplas76c.F
      !||    no_asso_plas76      ../engine/source/materials/mat/mat076/no_asso_plas76.F
      !||    no_asso_qplas76c    ../engine/source/materials/mat/mat076/no_asso_qplas76c.F
      !||    read_mat_table      ../engine/source/materials/tools/read_mat_table.F
      !||    table_mat_vinterp   ../engine/source/materials/tools/table_mat_vinterp.F
      !||    table_rresti_mat    ../engine/source/materials/tools/table_rresti_mat.F
      !||    table_slope         ../starter/source/materials/tools/table_slope.F
      !||    visc_param_mod      ../common_source/modules/mat_elem/visc_param_mod.F90
      !||    write_mat_table     ../engine/source/materials/tools/write_mat_table.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod           ../engine/share/modules/table_mod.F
      !||====================================================================
      MODULE TABLE4D_MOD
c
      USE TABLE_MOD
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C=======================================================================
      TYPE TABLE_4D_
          INTEGER :: NOTABLE
          INTEGER :: NDIM
          TYPE(TTABLE_XY), DIMENSION(:) ,ALLOCATABLE :: X
          my_real, DIMENSION(:)         ,ALLOCATABLE :: Y1D
          my_real, DIMENSION(:,:)       ,ALLOCATABLE :: Y2D
          my_real, DIMENSION(:,:,:)     ,ALLOCATABLE :: Y3D
          my_real, DIMENSION(:,:,:,:)   ,ALLOCATABLE :: Y4D
      END TYPE TABLE_4D_
C-----------------------------------------------
      END MODULE TABLE4D_MOD
