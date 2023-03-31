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
Chd|  ALEMUSCL_MOD                  modules/alemuscl_mod.F        
Chd|-- called by -----------
Chd|        CONTRL                        starter/source/starter/contrl.F
Chd|        HM_READ_ALE_MUSCL             starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_MAT37                 starter/source/materials/mat/mat037/hm_read_mat37.F
Chd|        HM_READ_MAT51                 starter/source/materials/mat/mat051/hm_read_mat51.F
Chd|        STARTER0                      starter/source/starter/starter0.F
Chd|        ST_QAPRINT_ALE_OPTIONS_DRIVER starter/source/output/qaprint/st_qaprint_ale_options_driver.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
Chd|        ALEMUSCL_DEALLOCATE           engine/source/ale/alemuscl/alemuscl_deallocate.F
Chd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
Chd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
Chd|        ALETHE                        engine/source/ale/alethe.F    
Chd|        FREFORM                       engine/source/input/freform.F 
Chd|        GRADIENT_LIMITATION           engine/source/ale/alemuscl/gradient_limitation.F
Chd|        GRADIENT_LIMITATION2          engine/source/ale/alemuscl/gradient_limitation2.F
Chd|        GRADIENT_RECONSTRUCTION       engine/source/ale/alemuscl/gradient_reconstruction.F
Chd|        GRADIENT_RECONSTRUCTION2      engine/source/ale/alemuscl/gradient_reconstruction2.F
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        ALE51_GRADIENT_RECONSTRUCTION2engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE ALEMUSCL_MOD
C-----------------------------------------------
        IMPLICIT NONE
#include      "my_real.inc"
C-----------------------------------------------
        TYPE ALEMUSCL_BUFFER_    
          my_real, DIMENSION(:, :), ALLOCATABLE    :: VOLUME_FRACTION
          my_real, DIMENSION(:, :), ALLOCATABLE    :: NODE_MAX_VALUE, NODE_MIN_VALUE
          my_real, DIMENSION(:, :, :), ALLOCATABLE :: GRAD
          my_real, DIMENSION(:, :), ALLOCATABLE    :: ELCENTER
          INTEGER, POINTER, DIMENSION(:)           :: pCNEL, pADDCNEL, pADDTMPL
        END TYPE
      
        TYPE ALEMUSCL_PARAM_
          my_real  BETA
          INTEGER  IALEMUSCL
          INTEGER  I_LAW
          INTEGER  I_MUSCL_OFF      
        END TYPE
      
        TYPE(ALEMUSCL_BUFFER_) :: ALEMUSCL_Buffer !Cell data
        TYPE(ALEMUSCL_PARAM_)  :: ALEMUSCL_Param  !global parameters
      
      END MODULE ALEMUSCL_MOD
