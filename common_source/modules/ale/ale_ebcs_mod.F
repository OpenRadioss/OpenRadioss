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
      !||    ale_ebcs_mod        ../common_source/modules/ale/ale_ebcs_mod.F
      !||--- called by ------------------------------------------------------
      !||    contrl              ../starter/source/starter/contrl.F
      !||    ddsplit             ../starter/source/restart/ddsplit/ddsplit.F
      !||    domdec2             ../starter/source/spmd/domdec2.F
      !||    fillcne             ../starter/source/spmd/domdec2.F
      !||    iniebcs             ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||    iniebcs_propergol   ../starter/source/boundary_conditions/ebcs/iniebcs_propergol.F90
      !||    iniebcsp0           ../starter/source/boundary_conditions/ebcs/iniebcsp0.F
      !||    lectur              ../engine/source/input/lectur.F
      !||    read_ebcs           ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    split_ebcs          ../starter/source/boundary_conditions/ebcs/split_ebcs.F
      !||    st_qaprint_driver   ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_ebcs     ../starter/source/output/qaprint/st_qaprint_ebcs.F
      !||    wrcomip             ../starter/source/restart/ddsplit/wrcommp.F
      !||====================================================================
      MODULE ALE_EBCS_MOD
        IMPLICIT NONE
#include      "my_real.inc"
      INTEGER NEBCS   

      END MODULE ALE_EBCS_MOD
