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
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
#include "my_real.inc"
c-----------------------------------------------------------------------
      MODULE LOADS_MOD
c-----------------------------------------------------------------------
      USE PLOAD_CYL_MOD
      USE DOMDEC_LOAD_MOD
c-----------------------------------------------------------------------
      TYPE LOADS_
        INTEGER :: NLOAD_CYL
        TYPE (PRESS_CYL_) ,DIMENSION(:) ,ALLOCATABLE :: LOAD_CYL
        TYPE (DOMDEC_LOAD_), DIMENSION(:), ALLOCATABLE :: CYL_RESTART
        INTEGER :: S_GLOBAL_SEGMENT_ID
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: GLOBAL_SEGMENT_ID
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: INDEX_LOAD ! index : global load id --> local load id
      END TYPE LOADS_
c-----------------------------------------------------------------------
      END MODULE LOADS_MOD
