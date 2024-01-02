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
Chd|  W_GJOINT                      source/restart/ddsplit/w_gjoint.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE W_GJOINT(GJBUFI,NGJOINT,LKJNI,NODLOCAL,LEN_IA)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LKJNI, NGJOINT, LEN_IA,
     .        GJBUFI(LKJNI,*), NODLOCAL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N0, N1, N2, N3
C
      DO I = 1, NGJOINT
        N0 = GJBUFI(3,I)
        N1 = GJBUFI(4,I)
        N2 = GJBUFI(5,I)
        N3 = GJBUFI(6,I)
        IF(N0 /= 0)GJBUFI(3,I)=NODLOCAL(N0)
        IF(N1 /= 0)GJBUFI(4,I)=NODLOCAL(N1)
        IF(N2 /= 0)GJBUFI(5,I)=NODLOCAL(N2)
        IF(N3 /= 0)GJBUFI(6,I)=NODLOCAL(N3)
      ENDDO
      CALL WRITE_I_C(GJBUFI,NGJOINT*LKJNI)
      LEN_IA = LEN_IA + NGJOINT*LKJNI
C
      RETURN
      END
