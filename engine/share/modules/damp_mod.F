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
      !||    damp_mod     ../engine/share/modules/damp_mod.F
      !||--- called by ------------------------------------------------------
      !||    rdcomi       ../engine/source/output/restart/rdcomm.F
      !||    rdresb       ../engine/source/output/restart/rdresb.F
      !||    resol_head   ../engine/source/engine/resol_head.F
      !||    restalloc    ../engine/source/output/restart/arralloc.F
      !||    wrcomi       ../engine/source/output/restart/wrcomm.F
      !||    wrrestp      ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      MODULE DAMP_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c
c=======================================================================      
c define structure for Damping
c=======================================================================      
C 
      INTEGER , ALLOCATABLE , DIMENSION(:) :: ID_DAMP_VREL ! ID of ∕DAMP/VREL in DAMP list
      INTEGER , ALLOCATABLE , DIMENSION(:) :: FR_DAMP_VREL ! SPMD communications for /DAMP/VREL
C
      INTEGER :: NDAMP_VREL      ! Number of /DAMP/VREL
      INTEGER :: NDAMP_VREL_RBYG ! Global number of /DAMP/VREL with rigid body
C
c---------------
      END MODULE  DAMP_MOD
