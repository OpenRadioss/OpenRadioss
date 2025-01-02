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
      !||    func2d_mod               ../starter/share/modules1/func2d_mod.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_func2d           ../starter/source/tools/curve/hm_read_func2d.F
      !||    hm_read_inimap2d         ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    ini_inimap2d             ../starter/stub/ini_inimap2d.F
      !||    initia                   ../starter/source/elements/initia/initia.F
      !||    lec_inimap2d_file        ../starter/source/initial_conditions/inimap/lec_inimap2d_file.F
      !||    lectur                   ../starter/source/starter/lectur.F
      !||    st_qaprint_driver        ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_model_tools   ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||====================================================================
      MODULE FUNC2D_MOD
#include "my_real.inc"   
      TYPE FUNC2D_STRUCT
      INTEGER :: ID, NPT, DIM
      my_real, DIMENSION(:, :), ALLOCATABLE :: XVAL
      my_real, DIMENSION(:, :), ALLOCATABLE :: FVAL
      END TYPE FUNC2D_STRUCT
      END MODULE FUNC2D_MOD
