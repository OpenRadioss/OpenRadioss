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
      !||    refsta_mod             ../starter/share/modules1/refsta_mod.F
      !||--- called by ------------------------------------------------------
      !||    contrl                 ../starter/source/starter/contrl.F
      !||    hm_read_refsta         ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    lecrefsta              ../starter/source/loads/reference_state/refsta/lecrefsta.F
      !||    st_qaprint_refsta      ../starter/source/output/qaprint/st_qaprint_refsta.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      MODULE REFSTA_MOD
        USE NAMES_AND_TITLES_MOD , ONLY : ncharline
#include "my_real.inc"

         CHARACTER(len=ncharline) :: REFSTA_FILENAME
         LOGICAL IS_REFSTA
         INTEGER RS0_FMT

      END MODULE REFSTA_MOD
