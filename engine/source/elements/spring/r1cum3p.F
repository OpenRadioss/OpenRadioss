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
!||    r1cum3p   ../engine/source/elements/spring/r1cum3p.F
!||--- called by ------------------------------------------------------
!||    rforc3    ../engine/source/elements/spring/rforc3.F
!||====================================================================
      SUBROUTINE R1CUM3P(
     1   FOR,     STI,     FSKY,    FSKYV,
     2   IADR,    FX1,     FX2,     FY1,
     3   FY2,     FZ1,     FZ2,     MX1,
     4   MX2,     MY1,     MY2,     MZ1,
     5   MZ2,     EX,      EY,      EZ,
     6   NEL,     NFT)
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
      INTEGER IADR(3,*)
C     REAL
      my_real
     .   FOR(*), STI(3,*),FSKYV(LSKY,8),FSKY(8,LSKY),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .   EX(MVSIZ),EY(MVSIZ),EZ(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, N, J
C     REAL
      my_real
     .   F11(MVSIZ), F21(MVSIZ), F31(MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
        F11(I)=EX(I)*FOR(I)
        F21(I)=EY(I)*FOR(I)
        F31(I)=EZ(I)*FOR(I)
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKYV(N,1)=F11(I)
          FSKYV(N,2)=F21(I)
          FSKYV(N,3)=F31(I)
          FX1(I) = -F11(I)
          FY1(I) = -F21(I)
          FZ1(I) = -F31(I)
          MX1(I) = ZERO
          MY1(I) = ZERO
          MZ1(I) = ZERO  
          FSKYV(N,7)=STI(1,I)
          N = IADR(2,II)
          FSKYV(N,1)=-F11(I)
          FSKYV(N,2)=-F21(I)
          FSKYV(N,3)=-F31(I)
          FX2(I) = F11(I)
          FY2(I) = F21(I)
          FZ2(I) = F31(I)
          MX2(I) = ZERO
          MY2(I) = ZERO
          MZ2(I) = ZERO  
          FSKYV(N,7)=STI(2,I)
        ENDDO
      ELSE
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKY(1,N)=F11(I)
          FSKY(2,N)=F21(I)
          FSKY(3,N)=F31(I)
          FX1(I) = -F11(I)
          FY1(I) = -F21(I)
          FZ1(I) = -F31(I)
          MX1(I) = ZERO
          MY1(I) = ZERO
          MZ1(I) = ZERO  
          FSKY(7,N)=STI(1,I)
          N = IADR(2,II)
          FSKY(1,N)=-F11(I)
          FSKY(2,N)=-F21(I)
          FSKY(3,N)=-F31(I)
          FX2(I) = F11(I)
          FY2(I) = F21(I)
          FZ2(I) = F31(I)
          MX2(I) = ZERO
          MY2(I) = ZERO
          MZ2(I) = ZERO  
          FSKY(7,N)=STI(2,I)
        ENDDO
      ENDIF
C
      RETURN
      END
