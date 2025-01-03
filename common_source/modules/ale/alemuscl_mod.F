Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    alemuscl_mod                     ../common_source/modules/ale/alemuscl_mod.F
      !||--- called by ------------------------------------------------------
      !||    afluxt                           ../engine/source/ale/ale51/afluxt.F
      !||    ale51_gradient_reconstruction    ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2   ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    alemuscl_deallocate              ../engine/source/ale/alemuscl/alemuscl_deallocate.F
      !||    alemuscl_upwind                  ../engine/source/ale/alemuscl/alemuscl_upwind.F
      !||    alemuscl_upwind2                 ../engine/source/ale/alemuscl/alemuscl_upwind2.F
      !||    alethe                           ../engine/source/ale/alethe.F
      !||    contrl                           ../starter/source/starter/contrl.F
      !||    freform                          ../engine/source/input/freform.F
      !||    gradient_limitation              ../engine/source/ale/alemuscl/gradient_limitation.F
      !||    gradient_limitation2             ../engine/source/ale/alemuscl/gradient_limitation2.F
      !||    gradient_reconstruction          ../engine/source/ale/alemuscl/gradient_reconstruction.F
      !||    gradient_reconstruction2         ../engine/source/ale/alemuscl/gradient_reconstruction2.F
      !||    hm_read_ale_muscl                ../starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
      !||    hm_read_mat37                    ../starter/source/materials/mat/mat037/hm_read_mat37.F
      !||    hm_read_mat51                    ../starter/source/materials/mat/mat051/hm_read_mat51.F
      !||    lectur                           ../engine/source/input/lectur.F
      !||    radioss2                         ../engine/source/engine/radioss2.F
      !||    rdcomi                           ../engine/source/output/restart/rdcomm.F
      !||    resol                            ../engine/source/engine/resol.F
      !||    resol_init                       ../engine/source/engine/resol_init.F
      !||    restalloc                        ../engine/source/output/restart/arralloc.F
      !||    st_qaprint_ale_options_driver    ../starter/source/output/qaprint/st_qaprint_ale_options_driver.F
      !||    starter0                         ../starter/source/starter/starter0.F
      !||    wrcomi                           ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                          ../starter/source/restart/ddsplit/wrcommp.F
      !||====================================================================
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
