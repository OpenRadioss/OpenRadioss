Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  INI_MATPARAM                  source/materials/mat/ini_matparam.F
Chd|-- called by -----------
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|-- calls ---------------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|====================================================================
      SUBROUTINE INI_MATPARAM(MATPARAM_TAB,NUMMAT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD
C============================================================================
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NUMMAT
      TYPE(MATPARAM_STRUCT_) , DIMENSION(NUMMAT) :: MATPARAM_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IMAT
c=======================================================================
      DO IMAT=1,NUMMAT
        MATPARAM_TAB(IMAT)%ILAW    = 0
        MATPARAM_TAB(IMAT)%MAT_ID  = 0
        MATPARAM_TAB(IMAT)%COMPRESSIBILITY = 3     ! elsto_plastic
        MATPARAM_TAB(IMAT)%SMSTR = 2               ! large strain
        MATPARAM_TAB(IMAT)%STRAIN_FORMULATION = 1  ! incremental
        MATPARAM_TAB(IMAT)%NTABLE = 0              ! number of local function tables
      END DO
c-----------
      RETURN
      END 
