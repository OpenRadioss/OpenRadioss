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
Chd|  TRI11                         share/modules/tri11_mod.F     
Chd|-- called by -----------
Chd|        I11BUCE_VOX                   source/interfaces/intsort/i11buce.F
Chd|        I11TRIVOX                     source/interfaces/intsort/i11trivox.F
Chd|        I25TRIVOX_EDG                 source/interfaces/intsort/i25trivox_edg.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE TRI11
#include "my_real.inc"

      INTEGER ::
     .        MIN_IX, MIN_IY, MIN_IZ,            !indice voxel min utilise
     .        MAX_IX, MAX_IY, MAX_IZ             !indice voxel max utilise

      INTEGER, POINTER, DIMENSION(:) :: LCHAIN_ELEM
      INTEGER, POINTER, DIMENSION(:) :: LCHAIN_NEXT
      INTEGER, POINTER, DIMENSION(:) :: LCHAIN_LAST 
      

      !INTEGER INIVOXEL11,VOXEL11(LVOXEL)
      !DATA INIVOXEL11 /1/   
                 
      END MODULE TRI11
