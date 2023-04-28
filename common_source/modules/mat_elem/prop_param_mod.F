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
Chd|  PROPPARAM_MOD              
Chd|-- called by -----------
Chd|====================================================================
      MODULE PROP_PARAM_MOD
        USE PLY_PARAM_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
#include "nchar_c.inc"
c=======================================================================      
      ! list of index values for IGEO   (integer array)
!      INTEGER ,PARAMETER  :: IGEO_IGTYP  = 
!      INTEGER ,PARAMETER  :: IGEO_ISMSTR = 
!      INTEGER ,PARAMETER  :: IGEO_IHBE   = 
!      INTEGER ,PARAMETER  :: IGEO_ISH3N  = 
!      INTEGER ,PARAMETER  :: IGEO_ISENS  = 
!      INTEGER ,PARAMETER  :: IGEO_ICPRE  =
!      INTEGER ,PARAMETER  :: IGEO_ICSTR  =
!      INTEGER ,PARAMETER  :: IGEO_ISORTH =
!      INTEGER ,PARAMETER  :: IGEO_IFRAM  =
!      INTEGER ,PARAMETER  :: IGEO_IREP   =
!      INTEGER ,PARAMETER  :: IGEO_ITHK   =
!      INTEGER ,PARAMETER  :: IGEO_IPLAST =
!      INTEGER ,PARAMETER  :: IGEO_ISVIS  =
!      INTEGER ,PARAMETER  :: IGEO_IINT   =
!      INTEGER ,PARAMETER  :: IGEO_IGMAT  =
!      INTEGER ,PARAMETER  :: IGEO_ISTACK =
!      INTEGER ,PARAMETER  :: IGEO_IECROU =
!      INTEGER ,PARAMETER  :: IGEO_NLAY   =
!c 
!      ! list of index values for GEO  (real array)
!      INTEGER ,PARAMETER  :: GEO_QA    =
!      INTEGER ,PARAMETER  :: GEO_QB    =
!      INTEGER ,PARAMETER  :: GEO_QH    =      
!      ! size of arrays 
!      INTEGER ,PARAMETER  :: NPROPG  = 
!      INTEGER ,PARAMETER  :: NPROPGI = 
c----------------------------------------------------------------------- 
c 
      TYPE PROP_PARAM_                      ! (NUMGEO)
        CHARACTER(len=nchartitle) :: TITLE  ! Property title
        INTEGER     :: PROP_ID
        INTEGER     :: NLAY   
        
        TYPE (PLY_PARAM_) ,DIMENSION(:) ,ALLOCATABLE :: PLY_PARAM ! NLAY
        
        INTEGER  ,DIMENSION(:) ,ALLOCATABLE :: IGEO               ! NPROPG
        my_real  ,DIMENSION(:) ,ALLOCATABLE :: GEO                ! NPROPGI
        
      END TYPE PROP_PARAM_
c
c---------------
      END MODULE PROP_PARAM_MOD
