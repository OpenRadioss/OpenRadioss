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
Chd|  INIMAP2D_MOD                  share/modules1/inimap2d_mod.F 
Chd|-- called by -----------
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        INI_INIMAP2D                  stub/ini_inimap2d.F           
Chd|        LEC_INIMAP2D_FILE             source/initial_conditions/inimap/lec_inimap2d_file.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_INITIAL_CONDITIONS source/output/qaprint/st_qaprint_initial_conditions.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INIMAP2D_MOD
#include "my_real.inc"
#include "nchar_c.inc"

        TYPE FUNC_DATA
          my_real, ALLOCATABLE, DIMENSION(:,:) :: RHO
          my_real, ALLOCATABLE, DIMENSION(:,:) :: VFRAC                 
          my_real, ALLOCATABLE, DIMENSION(:,:) :: PRES  
        END TYPE
        
        TYPE INIMAP2D_STRUCT
          INTEGER :: FORMULATION, ID, FUNC_VEL, NBMAT
          my_real :: FAC_VEL
          INTEGER, DIMENSION(:), ALLOCATABLE :: FUNC_ALPHA, FUNC_RHO, FUNC_PRES, FUNC_ENER 
          my_real, DIMENSION(:), ALLOCATABLE :: FAC_RHO, FAC_PRES_ENER
          INTEGER :: NODEID1, NODEID2, NODEID3
          INTEGER :: GRBRICID, GRQUADID, GRSH3NID
          INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNODE
          my_real, DIMENSION(3) :: VEC1, VEC2, VEC3

          CHARACTER*nchartitle :: TITLE
          LOGICAL :: FILE
          INTEGER :: NUM_CENTROIDS, NUM_NODE_VEL
          LOGICAL :: CORRECTLY_READ          

          !function - /INIMAP1D/FILE
          my_real, ALLOCATABLE, DIMENSION(:,:) :: XVAL, XVAL_V 
          my_real, ALLOCATABLE, DIMENSION(:,:) :: VEL
          TYPE(FUNC_DATA),DIMENSION(:),ALLOCATABLE :: SUBMAT         
          
        END TYPE INIMAP2D_STRUCT
        
      END MODULE INIMAP2D_MOD
