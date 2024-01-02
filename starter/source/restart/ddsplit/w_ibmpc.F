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

Chd|====================================================================
Chd|  W_IBMPC                       source/restart/ddsplit/w_ibmpc.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE W_IBMPC(IBMPC,NUMMPC,LMPC,NODLOCAL,LAG_SEC,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IBMPC(*), NODLOCAL(*), NUMMPC, LMPC, LAG_SEC, LEN_IA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C
      IF(LAG_SEC == 1) THEN
        DO I = 1, LMPC
          N = IBMPC(NUMMPC+I)
          IF(N /= 0) IBMPC(NUMMPC+I) = NODLOCAL(N)
        ENDDO
      END IF
      CALL WRITE_I_C(IBMPC,NUMMPC+3*LMPC)
      LEN_IA = LEN_IA + NUMMPC+3*LMPC
C
      RETURN
      END
