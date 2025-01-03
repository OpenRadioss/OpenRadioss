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
      !||    dtdc_mod      ../engine/share/modules/dtdc_mod.F
      !||--- called by ------------------------------------------------------
      !||    resol         ../engine/source/engine/resol.F
      !||    resol_init    ../engine/source/engine/resol_init.F
      !||    scumualpha    ../engine/source/elements/thickshell/solidec/scumualpha.F
      !||    scumualpha6   ../engine/source/elements/thickshell/solidec/scumualpha6.F
      !||====================================================================
      MODULE DTDC_MOD 
C----------------------------------------------------------------------- 
C    thick-shell dt of shell, DC : Dynamic,Condensation  
C----------------------------------------------------------------------- 
#include "my_real.inc"      
C=======================================================================
c DEFINE ARRAYS (integers arrays)
C=======================================================================
        integer  NTSHEGG 
        integer, DIMENSION(:) , POINTER ::  IENUNL   !  2*NTSHEG 
C-- 1:N_U (Upper), 2:N_L (Lower)  
        integer, DIMENSION(:) , POINTER ::  IAD_STSH,FR_STSH   !  on the front 
        integer, DIMENSION(:) , POINTER ::  IAD_RTSH,FR_RTSH   !  on the front 
C=======================================================================
c DEFINE ARRAYS (float arrays) : work arrays
C=======================================================================
         my_real
     .          , DIMENSION(:), ALLOCATABLE :: ALPHA_DC        ! (NUMNOD) workarray
c---------------
      END MODULE DTDC_MOD
