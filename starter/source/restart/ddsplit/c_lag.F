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
!||    c_lag     ../starter/source/restart/ddsplit/c_lag.F
!||--- called by ------------------------------------------------------
!||    ddsplit   ../starter/source/restart/ddsplit/ddsplit.F
!||====================================================================
      SUBROUTINE C_LAG(IPARI, NPRW, LAG_SEC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IPARI(NPARI,*), NPRW(*), LAG_SEC
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
C compatibilisation des options non supportees si NSPMD > 1
      DO I = 1, NINTER
        IF(IPARI(33,I)/=0)LAG_SEC=1
      END DO
      DO I = 1, NRWALL
        IF(NPRW(I+5*NRWALL)==1)LAG_SEC=1
      END DO
      IF(NBCSLAG+NGJOINT+NRBYLAG > 0)LAG_SEC=1
C NUMMPC + NFVLAG : ok (parallele SPMD)
C
      RETURN
      END
