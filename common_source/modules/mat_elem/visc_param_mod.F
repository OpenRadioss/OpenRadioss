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
Chd|  VISC_PARAM_MOD                modules/mat_elem/visc_param_mod.F
Chd|-- called by -----------
Chd|        MATPARAM_DEF_MOD              common_source/modules/mat_elem/matparam_def_mod.F
Chd|        HM_READ_VISC_LPRONY           starter/source/materials/visc/hm_read_visc_lprony.F
Chd|        HM_READ_VISC_PRONY            starter/source/materials/visc/hm_read_visc_prony.F
Chd|        WRITE_VISCPARAM               starter/source/materials/mat/write_viscparam.F
Chd|        READ_VISCPARAM                engine/source/output/restart/read_viscparam.F
Chd|        VISCMAIN                      engine/source/materials/visc/viscmain.F
Chd|        VISC_ET                       engine/source/elements/solid/solidez/visc_et.F
Chd|        VISC_PRONY                    engine/source/materials/visc/visc_prony.F
Chd|        VISC_PRONY_LSTRAIN            engine/source/materials/visc/visc_prony_lstrain.F
Chd|        WRITE_VISCPARAM               engine/source/output/restart/write_viscparam.F
Chd|-- calls ---------------
Chd|        NAMES_AND_TITLES_MOD          modules/names_and_titles_mod.F
Chd|        TABLE_MOD                     starter/share/modules1/table_mod.F
Chd|        TABLE_MOD                     engine/share/modules/table_mod.F
Chd|====================================================================
      MODULE VISC_PARAM_MOD
      USE TABLE4D_MOD
      USE NAMES_AND_TITLES_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c defines data structure for viscosity model parameters
c=======================================================================      
      
      TYPE VISC_PARAM_
        INTEGER     :: ILAW                   ! viscosity model type (number)
        CHARACTER(LEN=NCHARTITLE) :: TITLE    ! viscosity model input name
        INTEGER     :: NUPARAM                ! number of real value material paraameters
        INTEGER     :: NIPARAM                ! number of int value material parameters
        INTEGER     :: NUVAR                  ! number of internal state variables
        INTEGER     :: NFUNC                  ! number of local functions in material
        INTEGER     :: NTABLE                 ! number of local function tables
        
        my_real        ,DIMENSION(:) ,ALLOCATABLE :: UPARAM  ! NUPARAM
        INTEGER        ,DIMENSION(:) ,ALLOCATABLE :: IPARAM  ! NIPARAM
        INTEGER        ,DIMENSION(:) ,ALLOCATABLE :: FUNC    ! NFUNC
        TYPE(TABLE_4D_),DIMENSION(:) ,ALLOCATABLE :: TABLE   ! NTABLE
      
      END TYPE VISC_PARAM_   
c
c---------------
      END MODULE VISC_PARAM_MOD
