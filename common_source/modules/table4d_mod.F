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
Chd|  TABLE4D_MOD                     share/modules1/table_mod.F    
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
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
          my_real, DIMENSION(:)         ,POINTER :: Y1D
          my_real, DIMENSION(:,:)       ,POINTER :: Y2D
          my_real, DIMENSION(:,:,:)     ,POINTER :: Y3D
          my_real, DIMENSION(:,:,:,:)   ,POINTER :: Y4D
      END TYPE TABLE_4D_
C-----------------------------------------------
      END MODULE TABLE4D_MOD
