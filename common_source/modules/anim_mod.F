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
C Temporary requested module to introduce TANIM_STOP and TANIM_STOP0
C All REAL VARIABLES related to ANIM were moved in this module
C       (intially located in ./engine/share/includes/com04_c.inc and engine/share/includes/scr_14.inc) 
C COMMON BLOCK are no longer maintened
Chd|====================================================================
      MODULE ANIM_MOD
                  
#include "my_real.inc"              

        !REAL VARIABLES FOR ANIMATION FILES
        my_real TANIM, TANIM0            !start time
        my_real DTANIM, DTANIM0          !time frequency
        my_real TANIM_STOP, TANIM_STOP0  !stop time
        my_real TANIMSENS  
        my_real DTANISENS
              
        !INTEGER VARIABLES FOR ANIMATION FILES
        ! refer to scr14_c.inc
          
      END MODULE ANIM_MOD
