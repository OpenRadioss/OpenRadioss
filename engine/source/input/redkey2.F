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
!||    redkey2                ../engine/source/input/redkey2.F
!||--- uses       -----------------------------------------------------
!||    names_and_titles_mod   ../common_source/modules/names_and_titles_mod.F
!||====================================================================
      SUBROUTINE REDKEY2(KEY2,LK2,KEY0,NKEY,CARTE,IKEY)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD, ONLY: NCHARLINE100
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LK2, NKEY, IKEY
      CHARACTER KEY2*80
      CHARACTER(LEN=NCHARLINE100) :: CARTE
      CHARACTER KEY0(NKEY)*5
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER JK, I, J
      CHARACTER KEY1*80
C-----------------------------------------------
      KEY1=' '
      KEY2=' '
      JK=0
      J=0
      DO I=1,NCHARLINE100
        IF(CARTE(I:I)=='/')THEN
         JK=JK+1
         J=0
        ELSEIF(CARTE(I:I)/=' '.AND.JK==1)THEN
         J=J+1
         KEY1(J:J)=CARTE(I:I)
        ELSEIF(CARTE(I:I)/=' '.AND.JK==2)THEN
         J=J+1
         LK2=J
         KEY2(J:J)=CARTE(I:I)
        ENDIF
      ENDDO
C
      DO IKEY=1,NKEY
        IF(KEY1==KEY0(IKEY))RETURN
      ENDDO

      IKEY=0
C
      RETURN
      END
