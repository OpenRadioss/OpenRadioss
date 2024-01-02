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
Chd|  W_LGAU                        source/restart/ddsplit/w_lgau.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE W_LGAU(LGAUGE,NC,NODLOCAL,LEN_IA,CEL,CEP,PROC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, LEN_IA, PROC,
     .        LGAUGE(3,*), NODLOCAL(*),CEL(*),CEP(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N, LGAU(3,NC),P
C
      DO I = 1, NC
        N = LGAUGE(3,I)
        LGAU(1,I) = LGAUGE(1,I)
        LGAU(2,I) = LGAUGE(2,I)
        IF(N>0)THEN
          LGAU(3,I) = NODLOCAL(N)
        !!ELSE
        ELSEIF(N<0)THEN
          P = CEP(-N + NUMELS )
          IF(PROC == P)THEN
            LGAU(3,I)= -CEL(-N+NUMELS)
          ELSE
            LGAU(3,I)=0
          ENDIF
        ELSE
          LGAU(3,I)=0
        ENDIF
      ENDDO
      CALL WRITE_I_C(LGAU,3*NC)
      LEN_IA = LEN_IA + 3*NC
C
      RETURN
      END
