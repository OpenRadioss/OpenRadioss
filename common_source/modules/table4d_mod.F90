!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  TABLE4D_MOD                   modules/table4d_mod.F
!hd|-- called by -----------
!hd|        FAIL_PARAM_MOD                common_source/modules/mat_elem/fail_param_mod.F
!hd|        MATPARAM_DEF_MOD              common_source/modules/mat_elem/matparam_def_mod.F
!hd|        VISC_PARAM_MOD                common_source/modules/mat_elem/visc_param_mod.F
!hd|        LAW70_TABLE                   starter/source/materials/mat/mat070/law70_table.F
!hd|        TABLE_MAT_VINTERP             starter/source/materials/tools/table_mat_vinterp.F
!hd|        TABLE_SLOPE                   starter/source/materials/tools/table_slope.F
!hd|        WRITE_MAT_TABLE               starter/source/materials/tools/write_mat_table.F
!hd|        ASSO_PLAS76                   engine/source/materials/mat/mat076/asso_plas76.F
!hd|        ASSO_QPLAS76C                 engine/source/materials/mat/mat076/asso_qplas76c.F
!hd|        CONDAMAGE                     engine/source/materials/mat/mat190/condamage.F
!hd|        CONVERSION                    engine/source/materials/mat/mat190/conversion.F
!hd|        NO_ASSO_LPLAS76C              engine/source/materials/mat/mat076/no_asso_lplas76c.F
!hd|        NO_ASSO_PLAS76                engine/source/materials/mat/mat076/no_asso_plas76.F
!hd|        NO_ASSO_QPLAS76C              engine/source/materials/mat/mat076/no_asso_qplas76c.F
!hd|        READ_MAT_TABLE                engine/source/materials/tools/read_mat_table.F
!hd|        TABLE_MAT_VINTERP             engine/source/materials/tools/table_mat_vinterp.F
!hd|        TABLE_RRESTI_MAT              engine/source/materials/tools/table_rresti_mat.F
!hd|        WRITE_MAT_TABLE               engine/source/materials/tools/write_mat_table.F
!hd|-- calls ---------------
!hd|        TABLE_MOD                     starter/share/modules1/table_mod.F
!hd|        TABLE_MOD                     engine/share/modules/table_mod.F
!hd|====================================================================
      MODULE TABLE4D_MOD
!
        USE TABLE_MOD
!-----------------------------------------------
#include      "my_real.inc"
!-----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!=======================================================================
        TYPE TABLE_4D_
          INTEGER :: NOTABLE
          INTEGER :: NDIM
          TYPE(TTABLE_XY), DIMENSION(:) ,ALLOCATABLE :: X
          my_real, DIMENSION(:)         ,ALLOCATABLE :: Y1D
          my_real, DIMENSION(:,:)       ,ALLOCATABLE :: Y2D
          my_real, DIMENSION(:,:,:)     ,ALLOCATABLE :: Y3D
          my_real, DIMENSION(:,:,:,:)   ,ALLOCATABLE :: Y4D
        END TYPE TABLE_4D_
!-----------------------------------------------
      END MODULE TABLE4D_MOD
