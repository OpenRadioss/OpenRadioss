Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    rupture_mod             ../engine/share/modules/rupture_mod.F
      !||--- called by ------------------------------------------------------
      !||    fail_wind_frwave        ../engine/source/materials/fail/alter/fail_wind_frwave.F
      !||    fail_wind_frwave_init   ../engine/source/materials/fail/alter/fail_wind_frwave_init.F
      !||    forintc                 ../engine/source/elements/forintc.F
      !||====================================================================
        MODULE RUPTURE_MOD
#include "my_real.inc"
!$COMMENT
!       RUPTURE_MOD description :
!              RUPTURE_MOD is a general module for failure subroutine
!       RUPTURE_MOD organization :
!               - INDEX_OVER_50_CYCLES is a threadprivate index
!                 used in fail_wind_frwave routine
!$ENDCOMMENT
!   -------------------------
            INTEGER, DIMENSION(:), ALLOCATABLE :: INDEX_OVER_50_CYCLES
            INTEGER :: NGR_FAIL_WIND
!$OMP THREADPRIVATE (NGR_FAIL_WIND)
!   -------------------------
        END MODULE RUPTURE_MOD
