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
!||    qmassreal2ap   ../engine/source/elements/solid_2d/quad/qmassreal2ap.F
!||--- called by ------------------------------------------------------
!||    bforc2         ../engine/source/ale/bimat/bforc2.F
!||====================================================================
      SUBROUTINE QMASSREAL2AP(
     1   OFF,     RHO,     VNEW,    FSKYM,
     2   IADQ,    NEL,     NFT)
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
      INTEGER, INTENT(IN) :: NFT
C     REAL
      my_real
     .   OFF(*), RHO(*), VNEW(*), FSKYM(*)
      INTEGER IADQ(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I, II, K
C-----------------------------------------------
      DO I=1,NEL
       MASS(I)=RHO(I)*VNEW(I)*OFF(I)*FOURTH
      ENDDO
C
      DO I=1,NEL
C
         II=I+NFT
         K = IADQ(1,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
C
         K = IADQ(2,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
C
         K = IADQ(3,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
C
         K = IADQ(4,II)
         FSKYM(K)=FSKYM(K) + MASS(I)
      ENDDO
C
      RETURN
      END
