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
Chd|  FUNC2D_MOD                    share/modules1/func2d_mod.F   
Chd|-- called by -----------
Chd|        HM_READ_FUNC2D                source/tools/curve/hm_read_func2d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        INI_INIMAP2D                  stub/ini_inimap2d.F           
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        LEC_INIMAP2D_FILE             source/initial_conditions/inimap/lec_inimap2d_file.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_MODEL_TOOLS        source/output/qaprint/st_qaprint_model_tools.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE FUNC2D_MOD
#include "my_real.inc"   
      TYPE FUNC2D_STRUCT
      INTEGER :: ID, NPT, DIM
      my_real, DIMENSION(:, :), ALLOCATABLE :: XVAL
      my_real, DIMENSION(:, :), ALLOCATABLE :: FVAL
      END TYPE FUNC2D_STRUCT
      END MODULE FUNC2D_MOD
