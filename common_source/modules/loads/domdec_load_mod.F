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
#include "my_real.inc"
c-----------------------------------------------------------------------
Chd|====================================================================
Chd|  DOMDEC_LOAD_MOD               modules/loads/domdec_load_mod.F
Chd|-- called by -----------
Chd|        LOADS_MOD                     common_source/modules/loads/loads_mod.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE DOMDEC_LOAD_MOD
c-----------------------------------------------------------------------
        TYPE DOMDEC_PROC_LOAD_
            INTEGER :: LOCAL_SEGMENT_NUMBER ! number of segment on the proc I
            INTEGER :: S_LOCAL_SEGMENT ! size of LOCAL_SEGMENT array
            INTEGER ,DIMENSION(:) ,ALLOCATABLE :: LOCAL_SEGMENT ! list of segments defined on the proc I
        END TYPE DOMDEC_PROC_LOAD_
        TYPE DOMDEC_LOAD_
            ! ---------
            INTEGER, DIMENSION(:), ALLOCATABLE :: SEGMENT_TO_PROC ! processor where the segment is defined
            TYPE(DOMDEC_PROC_LOAD_) ,DIMENSION(:) ,ALLOCATABLE :: PROC ! data structure to save the segment per proc
            ! ---------
        END TYPE DOMDEC_LOAD_
c-----------------------------------------------------------------------
      END MODULE DOMDEC_LOAD_MOD
