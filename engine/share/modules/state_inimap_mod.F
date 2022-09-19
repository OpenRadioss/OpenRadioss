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
#include "my_real.inc"

Chd|====================================================================
Chd|  STATE_INIMAP_MOD              share/modules/state_inimap_mod.F
Chd|-- called by -----------
Chd|        FRESTAT                       source/input/frestat.F        
Chd|        GENSTAT                       source/output/sta/genstat.F   
Chd|        SPMD_STATE_INIMAP1D_EXCH_DATA source/output/sta/spmd_state_inimap1d_exch_data.F
Chd|        SPMD_STATE_INIMAP2D_EXCH_DATA source/output/sta/spmd_state_inimap2d_exch_data.F
Chd|        SPMD_STATE_INIMAP_EXCH_SIZ    source/output/sta/spmd_state_inimap_exch_siz.F
Chd|        STAT_INIMAP1D_FILE_SPMD       source/output/sta/stat_inimap1d_file_spmd.F
Chd|        STAT_INIMAP1D_SPMD            source/output/sta/stat_inimap1d_spmd.F
Chd|        STAT_INIMAP2D_FILE_SPMD       source/output/sta/stat_inimap2d_file_spmd.F
Chd|        STAT_INIMAP2D_SPMD            source/output/sta/stat_inimap2d_spmd.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE STATE_INIMAP_MOD

        INTEGER :: STATE_INIMAP_CALL_NUMBER

        !for each submaterial
        TYPE CELL_DATA
          my_real, ALLOCATABLE, DIMENSION(:) :: RHO
          my_real, ALLOCATABLE, DIMENSION(:) :: VFRAC
          my_real, ALLOCATABLE, DIMENSION(:) :: E
          my_real, ALLOCATABLE, DIMENSION(:) :: PRES
        END TYPE

        !for each SPMD domain
        TYPE MAP_STRUCT
          INTEGER :: NUM_CENTROIDS
          INTEGER :: NUM_POINTS
          INTEGER :: MLW
          INTEGER :: NSUBMAT

          my_real :: SHIFT_N
          my_real :: SHIFT_Ny
          my_real :: SHIFT_Nz

          my_real :: SHIFT_C
          my_real :: SHIFT_Cy
          my_real :: SHIFT_Cz

          my_real :: LENGTH

          my_real, ALLOCATABLE, DIMENSION(:)   :: POS_CENTROIDS
          my_real, ALLOCATABLE, DIMENSION(:)   :: POS2_CENTROIDS

          my_real, ALLOCATABLE, DIMENSION(:)   :: POS_NODES
          my_real, ALLOCATABLE, DIMENSION(:)   :: POS2_NODES

          my_real, ALLOCATABLE, DIMENSION(:)   :: VEL_NODES
          my_real, ALLOCATABLE, DIMENSION(:)   :: VEL2_NODES

          INTEGER, ALLOCATABLE, DIMENSION(:)   :: NODE_IDS
          INTEGER, ALLOCATABLE, DIMENSION(:)   :: CELL_IDS


          TYPE(CELL_DATA),DIMENSION(:),ALLOCATABLE :: SUBMAT
        END TYPE

        !for each SPMD domain
        TYPE(MAP_STRUCT), ALLOCATABLE, DIMENSION(:) :: STATE_INIMAP_BUF

        !flag
        LOGICAL IS_STAT_INIMAP_DETECTED
        LOGICAL IS_STAT_INIMAP_SINGLE
        LOGICAL IS_STAT_INIMAP_FILE
        LOGICAL IS_STAT_INIMAP_MSG_ALREADY_DISPLAYED
        LOGICAL IS_STAT_INIMAP_VP
        LOGICAL IS_STAT_INIMAP_VE

        INTEGER NCELL_MAX
        INTEGER NPTS_MAX

      END MODULE
