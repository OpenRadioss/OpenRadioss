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
C
!||====================================================================
!||    ani_txt     ../engine/source/output/anim/generate/ani_txt.F
!||--- called by ------------------------------------------------------
!||    genani      ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_c_c   ../common_source/tools/input_output/write_routtines.c
!||====================================================================
      SUBROUTINE ANI_TXT(TEXT,LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER TEXT(80)
      INTEGER LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER CTEXT(81)
C
      CTEXT(1    :MIN(LEN,80)) = ICHAR(TEXT(1:MIN(LEN,80)))
      CTEXT(LEN+1:81 ) = 0
C
      CALL WRITE_C_C(CTEXT,81)
C
      RETURN
      END
C
!||====================================================================
!||    ani_txt50   ../engine/source/output/anim/generate/ani_txt.F
!||--- called by ------------------------------------------------------
!||    genani      ../engine/source/output/anim/generate/genani.F
!||--- calls      -----------------------------------------------------
!||    write_c_c   ../common_source/tools/input_output/write_routtines.c
!||====================================================================
      SUBROUTINE ANI_TXT50(TEXT,LEN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      CHARACTER TEXT(80)
      INTEGER LEN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER CTEXT(81)

      CTEXT(1    :LEN) = ICHAR(TEXT(1:LEN))
      CTEXT(LEN+1:81 ) = 0
      CALL WRITE_C_C(CTEXT,50)
C
      RETURN
      END
