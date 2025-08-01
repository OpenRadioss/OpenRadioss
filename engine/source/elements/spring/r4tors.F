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
!||    r4tors      ../engine/source/elements/spring/r4tors.F
!||--- called by ------------------------------------------------------
!||    r23law113   ../engine/source/elements/spring/r23law113.F
!||    r23law114   ../engine/source/elements/spring/r23law114.F
!||    rforc3      ../engine/source/elements/spring/rforc3.F
!||--- uses       -----------------------------------------------------
!||    h3d_mod     ../engine/share/modules/h3d_mod.F
!||====================================================================
      SUBROUTINE R4TORS(
     1   FORX,    FORY,    FORZ,    XMOM,
     2   YMOM,    ZMOM,    TANI,    AL,
     3   H3D_DATA,NEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE H3D_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   FORX(*), FORY(*), FORZ(*), XMOM(*),
     .   YMOM(*), ZMOM(*),TANI(15,*),AL(MVSIZ)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF (ANIM_FT(1) /= 0 .OR. H3D_DATA%UND_FORC /= 0) THEN
        DO I=1,NEL
          TANI(1,I)= FORX(I)
          TANI(2,I)= FORY(I)
          TANI(3,I)= FORZ(I)
          TANI(4,I)= XMOM(I)
          TANI(5,I)= YMOM(I) - HALF*AL(I)*FORZ(I)
          TANI(6,I)= ZMOM(I) + HALF*AL(I)*FORY(I)
          TANI(7,I)= -XMOM(I)
          TANI(8,I)= -YMOM(I) - HALF*AL(I)*FORZ(I)
          TANI(9,I)= -ZMOM(I) + HALF*AL(I)*FORY(I)
        ENDDO
      ENDIF
C---
      RETURN
      END
