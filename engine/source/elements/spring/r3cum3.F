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
!||    r3cum3   ../engine/source/elements/spring/r3cum3.F
!||--- called by ------------------------------------------------------
!||    rforc3   ../engine/source/elements/spring/rforc3.F
!||====================================================================
      SUBROUTINE R3CUM3(
     1   F,       FOR,     STI,     STIFN,
     2   DF,      EX,      EY,      EZ,
     3   EX2,     EY2,     EZ2,     NC1,
     4   NC2,     NC3,     NEL)
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
      INTEGER NC1(*),NC2(*),NC3(*)
C     REAL
      my_real
     .   F(3,*), FOR(*),STI(3,*),STIFN(*),DF(*),
     .   EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),EX2(MVSIZ),EY2(MVSIZ),EZ2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   F11(MVSIZ), F21(MVSIZ), F31(MVSIZ),
     .   ALTMP
C-----------------------------------------------
C-------------------------
C     ASSEMBLE
C-------------------------
      DO I=1,NEL
        F11(I)=EX(I)*(FOR(I)+DF(I))
        F21(I)=EY(I)*(FOR(I)+DF(I))
        F31(I)=EZ(I)*(FOR(I)+DF(I))
      ENDDO
C
c ... note:  dF = E*dF + dE*F.
C
      DO I=1,NEL
        F(1,NC1(I))=F(1,NC1(I))+F11(I)
        F(2,NC1(I))=F(2,NC1(I))+F21(I)
        F(3,NC1(I))=F(3,NC1(I))+F31(I)
        STIFN(NC1(I))=STIFN(NC1(I))+STI(1,I)
        F(1,NC2(I))=F(1,NC2(I))-F11(I)
        F(2,NC2(I))=F(2,NC2(I))-F21(I)
        F(3,NC2(I))=F(3,NC2(I))-F31(I)
        STIFN(NC2(I))=STIFN(NC2(I))+TWO*STI(2,I)
      ENDDO
C
      DO I=1,NEL
        F11(I)=EX2(I)*(FOR(I)-DF(I))
        F21(I)=EY2(I)*(FOR(I)-DF(I))
        F31(I)=EZ2(I)*(FOR(I)-DF(I))
      ENDDO
C
c ... note:  dF = E*dF + dE*F.
C
      DO I=1,NEL
        F(1,NC3(I))=F(1,NC3(I))+F11(I)
        F(2,NC3(I))=F(2,NC3(I))+F21(I)
        F(3,NC3(I))=F(3,NC3(I))+F31(I)
        STIFN(NC3(I))=STIFN(NC3(I))+STI(3,I)
        F(1,NC2(I))=F(1,NC2(I))-F11(I)
        F(2,NC2(I))=F(2,NC2(I))-F21(I)
        F(3,NC2(I))=F(3,NC2(I))-F31(I)
      ENDDO
C---
      RETURN
      END
