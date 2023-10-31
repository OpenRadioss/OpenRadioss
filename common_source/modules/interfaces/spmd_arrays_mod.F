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
Chd|  SPMD_ARRAYS_MOD               modules/interfaces/spmd_arrays_mod.F
Chd|-- called by -----------
Chd|        INTERFACES_MOD                common_source/modules/interfaces/interfaces_mod.F
Chd|        CHECK_NEIGHBOURS_SURFACE_STATEengine/source/interfaces/interf/check_neighbours_surface_state.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE SPMD_ARRAYS_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
       implicit  none
!   -----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!   -----------------------------------------------

!   -------------------------

C----------------------------------------------
C   Global SPMD_ARRAYS activated by Contact
C---------------------------------------------

          TYPE SPMD_ARRAYS_
             INTEGER , DIMENSION(:), ALLOCATABLE :: FR_EDG      ! List of frontier edges of main segments of int25 contacts
             INTEGER , DIMENSION(:), ALLOCATABLE :: IAD_FREDG   ! Adress of frontier edges of main segments of int25 contacts per proc spmd
          END TYPE SPMD_ARRAYS_

C
        END MODULE SPMD_ARRAYS_MOD
