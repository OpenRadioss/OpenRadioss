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
!||    r3cum3p   ../engine/source/elements/spring/r3cum3p.F
!||--- called by ------------------------------------------------------
!||    rforc3    ../engine/source/elements/spring/rforc3.F
!||====================================================================
      SUBROUTINE R3CUM3P(
     1   FOR,     STI,     FSKY,    FSKYV,
     2   IADR,    DF,      EX,      EY,
     3   EZ,      EX2,     EY2,     EZ2,
     4   NEL,     NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
C     REAL
      my_real
     .   FOR(*), STI(3,*),FSKYV(LSKY,8),DF(*),
     .   FSKY(8,LSKY),EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ),
     .   EX2(MVSIZ),EY2(MVSIZ),EZ2(MVSIZ)
      INTEGER IADR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, N,  J
      my_real
     .   F11(MVSIZ), F21(MVSIZ), F31(MVSIZ)
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
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKYV(N,1)=F11(I)
          FSKYV(N,2)=F21(I)
          FSKYV(N,3)=F31(I)
          FSKYV(N,7)=STI(1,I)
          N = IADR(2,II)
          FSKYV(N,1)=-F11(I)
          FSKYV(N,2)=-F21(I)
          FSKYV(N,3)=-F31(I)
          FSKYV(N,7)=TWO*STI(2,I)
        ENDDO
      ELSE
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKY(1,N)=F11(I)
          FSKY(2,N)=F21(I)
          FSKY(3,N)=F31(I)
          FSKY(7,N)=STI(1,I)
          N = IADR(2,II)
          FSKY(1,N)=-F11(I)
          FSKY(2,N)=-F21(I)
          FSKY(3,N)=-F31(I)
          FSKY(7,N)=TWO*STI(2,I)
        ENDDO
      ENDIF
C
      DO I=1,NEL
        F11(I)=EX2(I)*(FOR(I)-DF(I))
        F21(I)=EY2(I)*(FOR(I)-DF(I))
        F31(I)=EZ2(I)*(FOR(I)-DF(I))
      ENDDO
C
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=1,NEL
C
          II = I+NFT
          N = IADR(2,II)
          FSKYV(N,1)=FSKYV(N,1)-F11(I)
          FSKYV(N,2)=FSKYV(N,2)-F21(I)
          FSKYV(N,3)=FSKYV(N,3)-F31(I)
          N = IADR(3,II)
          FSKYV(N,1)=F11(I)
          FSKYV(N,2)=F21(I)
          FSKYV(N,3)=F31(I)
          FSKYV(N,7)=STI(3,I)
        ENDDO
      ELSE
        DO I=1,NEL
          II = I+NFT
          N = IADR(2,II)
          FSKY(1,N)=FSKY(1,N)-F11(I)
          FSKY(2,N)=FSKY(2,N)-F21(I)
          FSKY(3,N)=FSKY(3,N)-F31(I)
          N = IADR(3,II)
          FSKY(1,N)=F11(I)
          FSKY(2,N)=F21(I)
          FSKY(3,N)=F31(I)
          FSKY(7,N)=STI(3,I)
        ENDDO
      ENDIF
C---
      RETURN
      END
