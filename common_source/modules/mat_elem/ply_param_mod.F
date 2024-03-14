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
Chd|====================================================================
Chd|  PLY_PARAM_MOD                 modules/mat_elem/ply_param_mod.F
Chd|-- called by -----------
Chd|        PROP_PARAM_MOD                common_source/modules/mat_elem/prop_param_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE PLY_PARAM_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      

      TYPE PLY_PARAM_                      ! NLAY
        my_real :: ALPHA                
        my_real :: PHI
        my_real :: THICK
        my_real :: POS
        my_real :: P_THKLY
        my_real :: P_WEIGHT
        INTEGER :: PID
        INTEGER :: MID_PLY
        INTEGER :: MID_INTPLY
        INTEGER :: NPT_PLY
      END TYPE PLY_PARAM_

c---------------
      END MODULE PLY_PARAM_MOD
