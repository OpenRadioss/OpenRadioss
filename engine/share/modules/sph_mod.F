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
C last SPH structures only (see RESTMOD for other SPH structures)
      !||====================================================================
      !||    sph_mod      ../engine/share/modules/sph_mod.F
      !||--- called by ------------------------------------------------------
      !||    rdresb       ../engine/source/output/restart/rdresb.F
      !||    resol_head   ../engine/source/engine/resol_head.F
      !||    restalloc    ../engine/source/output/restart/arralloc.F
      !||    wrrestp      ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE SPH_MOD
#include "my_real.inc"

        INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      SPH2SOL,SOL2SPH,IRST,SOL2SPH_TYP

        TYPE SPSYM_STRUCT
            my_real, DIMENSION(:)  , POINTER :: BUF 
        END TYPE SPSYM_STRUCT

      END MODULE SPH_MOD
