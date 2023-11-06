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
Chd|  FVSTATS                       source/airbag/fvstats.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F
Chd|-- calls ---------------
Chd|        FVSTATS1                      source/airbag/fvstats1.F
Chd|        SPMD_ALLGLOB_ISUM9            source/mpi/generic/spmd_allglob_isum9.F
Chd|        SPMD_CHKW                     source/mpi/generic/spmd_chkw.F
Chd|        SPMD_WIOUT                    source/mpi/generic/spmd_wiout.F
Chd|        FVBAG_MOD                     share/modules/fvbag_mod.F
Chd|====================================================================
      SUBROUTINE FVSTATS(MONVOL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE FVBAG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER MONVOL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IWIOUT
C
        IF(NFVBAG == 0) RETURN

        CALL FVSTATS1(IOUT,MONVOL,0)
C
        IF (NSPMD > 1) THEN
          IWIOUT = 0
          IF(ISPMD /= 0 ) CALL SPMD_CHKW(IWIOUT,IOUT)
          CALL SPMD_ALLGLOB_ISUM9(IWIOUT,1)
          CALL SPMD_WIOUT(IOUT,IWIOUT)
        ENDIF
C
        RETURN
      END

