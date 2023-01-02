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
Chd|  POINTERDEF                    share/modules/pointerdef_mod.F
Chd|-- called by -----------
Chd|        SPMD_I25_SLIDE_EXCH           source/mpi/interfaces/spmd_i25slide.F
Chd|        IFRONT_MOD                    share/modules/ifront_mod.F    
Chd|        INTERFACE_MODIFICATION_MOD    share/modules/interface_modification_mod.F
Chd|        TRI25EBOX                     share/modules/tri25ebox.F     
Chd|        TRI25TMP                      share/modules/tri25tmp_mod.F  
Chd|        TRI7BOX                       share/modules/tri7box.F       
Chd|-- calls ---------------
Chd|====================================================================
      MODULE POINTERDEF
#include "my_real.inc"
      TYPE int_pointer
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE int_pointer
      TYPE int_pointer2
        INTEGER, DIMENSION(:,:), POINTER :: P
      END TYPE int_pointer2
      TYPE real_pointer
        my_real, DIMENSION(:), POINTER :: P
      END TYPE real_pointer
      TYPE real_pointer2
        my_real, DIMENSION(:,:), POINTER :: P
      END TYPE real_pointer2
      TYPE real_pointer3
        my_real, DIMENSION(:,:,:), POINTER :: P
      END TYPE real_pointer3
      TYPE real4_pointer3
        REAL*4, DIMENSION(:,:,:), POINTER :: P
      END TYPE real4_pointer3
      TYPE r8_pointer
        DOUBLE PRECISION, DIMENSION(:), POINTER :: P
      END TYPE r8_pointer
      TYPE r8_pointer3
        DOUBLE PRECISION, DIMENSION(:,:,:), POINTER :: P
      END TYPE r8_pointer3
      END MODULE POINTERDEF

