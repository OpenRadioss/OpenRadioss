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
Chd|  FAIL_PARAM_MOD                modules/mat_elem/fail_param_mod.F
Chd|-- called by -----------
Chd|        MATPARAM_DEF_MOD              common_source/modules/mat_elem/matparam_def_mod.F
Chd|        WRITE_FAILPARAM               starter/source/materials/fail/write_failparam.F
Chd|        READ_FAILPARAM                engine/source/output/restart/read_failparam.F
Chd|        WRITE_FAILPARAM               engine/source/output/restart/write_failparam.F
Chd|-- calls ---------------
Chd|        NAMES_AND_TITLES_MOD          modules/names_and_titles_mod.F
Chd|        TABLE_MOD                     starter/share/modules1/table_mod.F
Chd|        TABLE_MOD                     engine/share/modules/table_mod.F
Chd|====================================================================
      MODULE FAIL_PARAM_MOD
c
      USE TABLE_MOD
      USE NAMES_AND_TITLES_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c define type FAIL_ for general failure model data storage
c=======================================================================      
      
      TYPE FAIL_PARAM_
        character(len=nchartitle) :: KEYWORD  ! failure model type name
        INTEGER     :: IRUPT                  ! failure model type (number)
        INTEGER     :: FAIL_ID
        INTEGER     :: NUPARAM
        INTEGER     :: NIPARAM
        INTEGER     :: NUVAR
        INTEGER     :: NFUNC
        INTEGER     :: NTABLE
        INTEGER     :: NMOD                   !number of rupture/damage modes

        INTEGER     :: FAIL_IP                
        my_real     :: PTHK
        
        character(len=nchartitle) ,DIMENSION(:) ,ALLOCATABLE :: MODE   ! size NMOD
        my_real       ,DIMENSION(:) ,ALLOCATABLE :: UPARAM  ! NUPARAM
        INTEGER       ,DIMENSION(:) ,ALLOCATABLE :: IPARAM  ! NIPARAM
        INTEGER       ,DIMENSION(:) ,ALLOCATABLE :: IFUNC   ! function adress table
        TYPE (TTABLE) ,DIMENSION(:) ,ALLOCATABLE :: TABLE   ! NTABLE
      
      END TYPE FAIL_PARAM_   
c
c---------------
      END MODULE FAIL_PARAM_MOD
