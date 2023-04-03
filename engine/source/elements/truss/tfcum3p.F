Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  TFCUM3P                       source/elements/truss/tfcum3p.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TFCUM3P(
     1   FOR,     STI,     FSKY,    FSKYV,
     2   IADT,    FX1,     FX2,     FY1,
     3   FY2,     FZ1,     FZ2,     OFFG,
     4   OFF,     NC1,     NC2,     EX,
     5   EY,      EZ,      NEL,     NFT)
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
      my_real
     .   FSKYV(LSKY,8),FSKY(8,LSKY),FOR(*),STI(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   OFFG(MVSIZ),OFF(MVSIZ),EX(MVSIZ),EY(MVSIZ),
     .   EZ(MVSIZ)
      INTEGER IADT(2,*),NC1(MVSIZ),NC2(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,J
      my_real
     .   F11(MVSIZ),F21(MVSIZ),F31(MVSIZ),OFF_L
C-----------------------------------------------      
      DO I=1,NEL
        F11(I)=EX(I)*FOR(I)
        F21(I)=EY(I)*FOR(I)
        F31(I)=EZ(I)*FOR(I)
      ENDDO
c
      OFF_L=ZERO
      DO I=1,NEL
        IF (OFF(I) < ONE) OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
c
      IF (OFF_L < ZERO) THEN
        DO I=1,NEL
          IF(OFFG(I) < ZERO) THEN
            F11(I)=ZERO
            F21(I)=ZERO
            F31(I)=ZERO
            STI(I)=ZERO
          ENDIF
        ENDDO
      ENDIF
C-------------------------
C     ASSEMBLE
C-------------------------
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=1,NEL
          II = I+NFT
          K = IADT(1,II)
          FSKYV(K,1)=F11(I)
          FSKYV(K,2)=F21(I)
          FSKYV(K,3)=F31(I)
          FSKYV(K,7)=STI(I)
          FX1(I) = -F11(I)
          FY1(I) = -F21(I)
          FZ1(I) = -F31(I)
          K = IADT(2,II)
          FSKYV(K,1)=-F11(I)
          FSKYV(K,2)=-F21(I)
          FSKYV(K,3)=-F31(I)
          FSKYV(K,7)=STI(I)
          FX2(I) = F11(I)
          FY2(I) = F21(I)
          FZ2(I) = F31(I)
        ENDDO
      ELSE
        DO I=1,NEL
         II = I+NFT
         K = IADT(1,II)
         FSKY(1,K)=F11(I)
         FSKY(2,K)=F21(I)
         FSKY(3,K)=F31(I)
         FSKY(7,K)=STI(I)
         FX1(I) = -F11(I)
         FY1(I) = -F21(I)
         FZ1(I) = -F31(I)
         K = IADT(2,II)
         FSKY(1,K)=-F11(I)
         FSKY(2,K)=-F21(I)
         FSKY(3,K)=-F31(I)
         FSKY(7,K)=STI(I)
         FX2(I) = F11(I)
         FY2(I) = F21(I)
         FZ2(I) = F31(I)
        ENDDO
      ENDIF
C-----------------------------------------------        
      RETURN
      END
