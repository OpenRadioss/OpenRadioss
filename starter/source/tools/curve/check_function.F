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
!||    check_function_id       ../starter/source/tools/curve/check_function.F
!||--- called by ------------------------------------------------------
!||    hm_read_monvol_type10   ../starter/source/airbag/hm_read_monvol_type10.F
!||====================================================================
      SUBROUTINE CHECK_FUNCTION_ID(NPC, NFUNCT, IFUNCT_IN, IFUNCT_OUT, IFOUND)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NPC(*), NFUNCT, IFUNCT_IN, IFUNCT_OUT
      LOGICAL, INTENT(OUT) :: IFOUND
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C--------------------------------------------------------------------------       
      IFOUND = .FALSE.
      IFUNCT_OUT = 0
      DO I=1,NFUNCT
         IF(IFUNCT_IN == NPC(I)) THEN
            IFUNCT_OUT = I
            IFOUND = .TRUE.
            EXIT
         ENDIF
      ENDDO
      RETURN
      END
