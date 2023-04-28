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
Chd|  GROUP_PARAM              
Chd|-- called by -----------
Chd|====================================================================
      MODULE GROUP_PARAM_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c define type GROUP_PARAM_STRUCT_ for element group parameters depending on property/material
c allocatable dimension : NGROUP
c=======================================================================      
!      INTEGER ,PARAMETER  :: IPARG_X = 
!      INTEGER ,PARAMETER  :: IPARG_Y = 
!      INTEGER ,PARAMETER  :: IPARG_Z = 

!      INTEGER ,PARAMETER  :: NPARG  = 100
c 
      TYPE GROUP_PARAM_               ! (NGROUP)
        integer     :: IMAT    
        integer     :: IPROP
        integer     :: NPARG    
        my_real     :: VISC_DN
        my_real     :: VISC_DM
        integer     :: ISMSTR
        
        integer ,DIMENSION(:) ,ALLOCATABLE :: IPARG  ! size = NPARG

      END TYPE GROUP_PARAM_
c
c---------------
      END MODULE GROUP_PARAM_MOD
