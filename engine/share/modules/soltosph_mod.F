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
!||    soltosph_mod   ../engine/share/modules/soltosph_mod.F
!||--- called by ------------------------------------------------------
!||    radioss2       ../engine/source/engine/radioss2.F
!||    resol_head     ../engine/source/engine/resol_head.F
!||    soltosph_on2   ../engine/source/elements/sph/soltosph_on2.F
!||    soltospha      ../engine/source/elements/sph/soltospha.F
!||    soltosphf      ../engine/source/elements/sph/soltosph.F
!||====================================================================
      MODULE SOLTOSPH_MOD
      implicit none
#include      "my_real.inc"
      my_real
     .       , DIMENSION(:,:,:), ALLOCATABLE :: WPARTSAV
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: AWORK
      my_real,
     .        DIMENSION(:), ALLOCATABLE :: DMSPH
      INTEGER,DIMENSION(:), ALLOCATABLE :: WSPCLOUD
C=======================================================================
      END MODULE SOLTOSPH_MOD
