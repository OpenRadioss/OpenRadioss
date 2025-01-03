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
      !||    fxfluxrrest      ../engine/source/constraints/thermic/fxfluxrrest.F
      !||--- called by ------------------------------------------------------
      !||    rdresb           ../engine/source/output/restart/rdresb.F
      !||--- calls      -----------------------------------------------------
      !||    read_db          ../common_source/tools/input_output/read_db.F
      !||    read_i_c         ../common_source/tools/input_output/write_routtines.c
      !||    redsqi           ../engine/source/output/tools/redsqi.F
      !||    redsqr           ../engine/source/output/tools/redsqr.F
      !||--- uses       -----------------------------------------------------
      !||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||====================================================================
      SUBROUTINE FXFLUXRREST(IBFFLUX, FBFFLUX, GLOB_THERM)

      use glob_therm_mod
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------
      INTEGER IBFFLUX(*)
      my_real FBFFLUX(*)
      type (glob_therm_), INTENT(IN) :: GLOB_THERM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C
      IF (IRFORM/5<=1) THEN
        CALL REDSQR (FBFFLUX,GLOB_THERM%LFACTHER*GLOB_THERM%NFXFLUX,IRFORM)
        CALL REDSQI (IBFFLUX,GLOB_THERM%NITFLUX *GLOB_THERM%NFXFLUX,IRFORM)
      ELSE
        CALL READ_DB(FBFFLUX,GLOB_THERM%LFACTHER*GLOB_THERM%NFXFLUX)
        CALL READ_I_C(IBFFLUX,GLOB_THERM%NITFLUX*GLOB_THERM%NFXFLUX)
      ENDIF
C
      RETURN
      END
