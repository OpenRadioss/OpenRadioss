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
      !||    timfun                ../engine/source/tools/curve/timfun.F
      !||--- called by ------------------------------------------------------
      !||    resol                 ../engine/source/engine/resol.F
      !||--- calls      -----------------------------------------------------
      !||    interp                ../engine/source/tools/curve/interp.F
      !||--- uses       -----------------------------------------------------
      !||    python_funct_mod      ../common_source/modules/python_mod.F90
      !||====================================================================
      SUBROUTINE TIMFUN (PYTHON, FV,NP,TF)
            USE PYTHON_FUNCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include "tabsiz_c.inc"
#include      "com08_c.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(PYTHON_), INTENT(inout) :: PYTHON
      INTEGER NP(SNPC)
C     REAL
      my_real
     .   FV(*), TF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, NPOINT,ISMOOTH
C     REAL
      my_real
     .   TG
C-----------------------------------------------
      DO N=1,NFUNCT
        ISMOOTH = NP(2*NFUNCT+N+1)
        IF(ISMOOTH >= 0) THEN
          NPOINT=(NP(N+1)-NP(N))/2
          CALL INTERP(TF(NP(N)),TT,NPOINT,FV(N),TG)
        ELSE 
          ISMOOTH = -ISMOOTH
          CALL PYTHON_CALL_FUNCT1D(PYTHON, ISMOOTH,TT, FV(N))
        ENDIF
      ENDDO
      RETURN
      END
