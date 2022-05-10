Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  I25_FIE_MOD                   share/modules1/i25_fie_mod.F  
Chd|-- called by -----------
Chd|        PREPARE_SPLIT_I25E2E          source/spmd/prepare_split_i25e2e.F
Chd|        SPLIT_CAND_I25_EDGE           source/restart/ddsplit/inter_tools.F
Chd|        W_FI                          source/restart/ddsplit/w_fi.F 
Chd|-- calls ---------------
Chd|====================================================================
      MODULE I25_FIE_MOD 
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c
      TYPE I25_CAND_
C candidates E2E
        integer :: NB_CAND_E2E 
        integer, DIMENSION(:), ALLOCATABLE :: ID_E2E 
        integer, DIMENSION(:), ALLOCATABLE :: CANDS_E2E               
        integer, DIMENSION(:), ALLOCATABLE :: CANDM_E2E
C candidates E2S
        integer :: NB_CAND_E2S 
        integer, DIMENSION(:), ALLOCATABLE :: ID_E2S 
        integer, DIMENSION(:), ALLOCATABLE :: CANDS_E2S               
        integer, DIMENSION(:), ALLOCATABLE :: CANDM_E2S
      END TYPE I25_CAND_
      TYPE I25_FIE_
         integer :: NEDGE_TOT
         integer, DIMENSION(:), ALLOCATABLE :: NEDGE
         integer, DIMENSION(:), ALLOCATABLE :: ID
      END TYPE I25_FIE_

      TYPE(I25_CAND_), DIMENSION(:,:), ALLOCATABLE :: I25_SPLIT_CAND
      TYPE(I25_FIE_), DIMENSION(:,:), ALLOCATABLE :: I25_FIE

c
c---------------
      END MODULE I25_FIE_MOD
