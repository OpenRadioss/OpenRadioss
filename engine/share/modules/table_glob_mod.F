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
      !||    table_glob_mod         ../engine/share/modules/table_glob_mod.F
      !||--- called by ------------------------------------------------------
      !||    get_table_value        ../engine/source/user_interface/utable.F
      !||    get_table_value_dydx   ../engine/source/user_interface/utable.F
      !||    get_u_numtable         ../engine/source/user_interface/utable.F
      !||    get_u_table            ../engine/source/user_interface/utable.F
      !||    get_u_vtable           ../engine/source/user_interface/utable.F
      !||    get_vtable_value       ../engine/source/user_interface/utable.F
      !||    radioss2               ../engine/source/engine/radioss2.F
      !||    rdresb                 ../engine/source/output/restart/rdresb.F
      !||    resol_head             ../engine/source/engine/resol_head.F
      !||    wrrestp                ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod              ../engine/share/modules/table_mod.F
      !||====================================================================
      MODULE TABLE_GLOB_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE TABLE_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE(TTABLE), DIMENSION(:), ALLOCATABLE :: TABLE
      END MODULE TABLE_GLOB_MOD
