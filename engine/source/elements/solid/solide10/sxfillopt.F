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
!||    sxfillopt   ../engine/source/elements/solid/solide10/sxfillopt.F
!||--- called by ------------------------------------------------------
!||    s10forc3    ../engine/source/elements/solid/solide10/s10forc3.F
!||    s16forc3    ../engine/source/elements/thickshell/solide16/s16forc3.F
!||    s20forc3    ../engine/source/elements/solid/solide20/s20forc3.F
!||====================================================================
      SUBROUTINE SXFILLOPT(
     1   NPE,     FILL,    STI,     FX,
     2   FY,      FZ,      NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER NPE
      my_real
     .   FILL(*),STI(*),
     .   FX(MVSIZ,NPE), FY(MVSIZ,NPE), FZ(MVSIZ,NPE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------------------------
      DO I=1,NEL
        STI(I)=FILL(I)*STI(I)
      END DO              
      DO J=1,NPE
        DO I=1,NEL
          FX(I,J)=FILL(I)*FX(I,J)
          FY(I,J)=FILL(I)*FY(I,J)
          FZ(I,J)=FILL(I)*FZ(I,J)
        END DO              
      END DO              

      RETURN
      END
