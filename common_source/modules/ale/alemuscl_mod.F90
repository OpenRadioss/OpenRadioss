!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  ALEMUSCL_MOD                  modules/ale/alemuscl_mod.F
!hd|-- called by -----------
!hd|        CONTRL                        starter/source/starter/contrl.F
!hd|        HM_READ_ALE_MUSCL             starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
!hd|        HM_READ_MAT37                 starter/source/materials/mat/mat037/hm_read_mat37.F
!hd|        HM_READ_MAT51                 starter/source/materials/mat/mat051/hm_read_mat51.F
!hd|        STARTER0                      starter/source/starter/starter0.F
!hd|        ST_QAPRINT_ALE_OPTIONS_DRIVER starter/source/output/qaprint/st_qaprint_ale_options_driver.F
!hd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
!hd|        AFLUXT                        engine/source/ale/ale51/afluxt.F
!hd|        ALE51_GRADIENT_RECONSTRUCTION engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
!hd|        ALEMUSCL_DEALLOCATE           engine/source/ale/alemuscl/alemuscl_deallocate.F
!hd|        ALEMUSCL_UPWIND               engine/source/ale/alemuscl/alemuscl_upwind.F
!hd|        ALEMUSCL_UPWIND2              engine/source/ale/alemuscl/alemuscl_upwind2.F
!hd|        ALETHE                        engine/source/ale/alethe.F
!hd|        FREFORM                       engine/source/input/freform.F
!hd|        GRADIENT_LIMITATION           engine/source/ale/alemuscl/gradient_limitation.F
!hd|        GRADIENT_LIMITATION2          engine/source/ale/alemuscl/gradient_limitation2.F
!hd|        GRADIENT_RECONSTRUCTION       engine/source/ale/alemuscl/gradient_reconstruction.F
!hd|        GRADIENT_RECONSTRUCTION2      engine/source/ale/alemuscl/gradient_reconstruction2.F
!hd|        LECTUR                        engine/source/input/lectur.F
!hd|        RADIOSS2                      engine/source/engine/radioss2.F
!hd|        RDCOMI                        engine/source/output/restart/rdcomm.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESOL_INIT                    engine/source/engine/resol_init.F
!hd|        RESTALLOC                     engine/source/output/restart/arralloc.F
!hd|        WRCOMI                        engine/source/output/restart/wrcomm.F
!hd|        ALE51_GRADIENT_RECONSTRUCTION2engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE ALEMUSCL_MOD
!-----------------------------------------------
        IMPLICIT NONE
#include      "my_real.inc"
!-----------------------------------------------
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
