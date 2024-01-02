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
C
Chd|====================================================================
Chd|  C_IBMPC                       source/restart/ddsplit/c_ibmpc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C_IBMPC(IMPCNC,IMPCNN,IMPCDL,IMPCSKW,DD_LAGF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IMPCNC(*),IMPCNN(*),IMPCDL(*),IMPCSKW(*),DD_LAGF(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, P, NUMC, KF, J, ISK
C
      P = 1   ! uniquement sur p0 pour le moment
      KF = 0
      DO I=1,NUMMPC
        DD_LAGF(1,P) = DD_LAGF(1,P) + 1
        NUMC = IMPCNC(I)
        DO J=1,NUMC
          KF  = KF+1
          ISK = IMPCSKW(KF)
          IF(ISK==1)THEN
            DD_LAGF(2,P) = DD_LAGF(2,P) + 1
          ELSE
            DD_LAGF(2,P) = DD_LAGF(2,P) + 3
          END IF
        END DO
      END DO
C
      RETURN
      END
