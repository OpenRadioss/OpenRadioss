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
      !||    trimat_mod                       ../engine/share/modules/trimat.F
      !||--- called by ------------------------------------------------------
      !||    afluxt                           ../engine/source/ale/ale51/afluxt.F
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    restalloc                        ../engine/source/output/restart/arralloc.F
      !||====================================================================
      MODULE TRIMAT_MOD
#include "my_real.inc"
C tableau tempo entier partage
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: N4_VOIS
C tableau tempo reel partage
      my_real       ,DIMENSION(:,:), ALLOCATABLE :: FLUX_VOIS
      END MODULE TRIMAT_MOD
