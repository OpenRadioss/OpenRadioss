Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||    pointerdef                   ../engine/share/modules/pointerdef_mod.F
!||--- called by ------------------------------------------------------
!||    ifront_mod                   ../engine/share/modules/ifront_mod.F
!||    interface_modification_mod   ../engine/share/modules/interface_modification_mod.F
!||    spmd_i25_slide_exch          ../engine/source/mpi/interfaces/spmd_i25slide.F
!||    tri25ebox                    ../engine/share/modules/tri25ebox.F
!||    tri25tmp                     ../engine/share/modules/tri25tmp_mod.F
!||    tri7box                      ../engine/share/modules/tri7box.F
!||====================================================================
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

