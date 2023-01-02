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
Chd|  SOLTOSPH_MOD                  share/modules/soltosph_mod.F  
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        SOLTOSPHA                     source/elements/sph/soltospha.F
Chd|        SOLTOSPHF                     source/elements/sph/soltosph.F
Chd|        SOLTOSPH_ON2                  source/elements/sph/soltosph_on2.F
Chd|-- calls ---------------
Chd|====================================================================
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
