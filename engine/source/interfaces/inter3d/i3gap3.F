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
!||    i3gap3    ../engine/source/interfaces/inter3d/i3gap3.F
!||--- called by ------------------------------------------------------
!||    i5ke3     ../engine/source/interfaces/inter3d/i5ke3.F
!||    i6main    ../engine/source/interfaces/inter3d/i6main.F
!||    i9main3   ../engine/source/interfaces/int09/i9main3.F
!||    intvo3    ../engine/source/interfaces/inter3d/intvo3.F
!||    ud_int5   ../engine/source/implicit/upd_glob_k.F
!||====================================================================
      SUBROUTINE I3GAP3(
     1   GAP,     AREA,    THK,     ALP,
     2   LFT,     LLT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      my_real
     .   GAP
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  AREA
      my_real, DIMENSION(MVSIZ), INTENT(OUT) ::  THK,ALP
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      my_real
     .   VAR
C-----------------------------------------------
C
      DO I=LFT,LLT
        THK(I) = GAP
        ALP(I) = ABS(TWO*GAP/SQRT(AREA(I)))
        VAR    = FIVEEM2
        ALP(I) = MAX(VAR,ALP(I))
        VAR    = ONE_FIFTH
        ALP(I) = MIN(VAR,ALP(I))
      ENDDO
C
      RETURN
      END
