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
Chd|  PFCUM3P                       source/elements/beam/pfcum3p.F
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PFCUM3P(
     1   STI,     FSKY,    FSKYV,   IADP,
     2   FX1,     FX2,     FY1,     FY2,
     3   FZ1,     FZ2,     NC1,     NC2,
     4   NC3,     E1X,     E1Y,     E1Z,
     5   E2X,     E2Y,     E2Z,     E3X,
     6   E3Y,     E3Z,     F11,     F12,
     7   F21,     F22,     F31,     F32,
     8   FPHI,    FTHESKY, NEL,     NFT,
     9   JTHE)
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
      INTEGER, INTENT(IN) :: JTHE
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ)
      my_real
     .   FSKYV(LSKY,8),FSKY(8,LSKY),STI(*),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),
     .   E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   F11(MVSIZ),F12(MVSIZ),F21(MVSIZ),
     .   F22(MVSIZ),F31(MVSIZ),F32(MVSIZ),
     .   FPHI(MVSIZ,2),FTHESKY(*)
      INTEGER IADP(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K
      my_real
     .   G11(MVSIZ),G12(MVSIZ),G21(MVSIZ), 
     .   G22(MVSIZ),G31(MVSIZ),G32(MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
        G11(I)=F11(I)
        G21(I)=F21(I)
        G31(I)=F31(I)
        G12(I)=F12(I)
        G22(I)=F22(I)
        G32(I)=F32(I)
      ENDDO
C
      DO I=1,NEL
        F11(I)=E1X(I)*G11(I)+E2X(I)*G21(I)+E3X(I)*G31(I)
        F21(I)=E1Y(I)*G11(I)+E2Y(I)*G21(I)+E3Y(I)*G31(I)
        F31(I)=E1Z(I)*G11(I)+E2Z(I)*G21(I)+E3Z(I)*G31(I)
        F12(I)=E1X(I)*G12(I)+E2X(I)*G22(I)+E3X(I)*G32(I)
        F22(I)=E1Y(I)*G12(I)+E2Y(I)*G22(I)+E3Y(I)*G32(I)
        F32(I)=E1Z(I)*G12(I)+E2Z(I)*G22(I)+E3Z(I)*G32(I)
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      IF (IVECTOR==1) THEN
#include "vectorize.inc"
        DO I=1,NEL
          II = I +NFT
          K = IADP(1,II)
          FSKYV(K,1)=-F11(I)
          FSKYV(K,2)=-F21(I)
          FSKYV(K,3)=-F31(I)
          FX1(I) = F11(I)
          FY1(I) = F21(I)
          FZ1(I) = F31(I)
          FSKYV(K,7)=STI(I)
          K = IADP(2,II)
          FSKYV(K,1)=-F12(I)
          FSKYV(K,2)=-F22(I)
          FSKYV(K,3)=-F32(I)
          FX2(I) = F12(I)
          FY2(I) = F22(I)
          FZ2(I) = F32(I)
          FSKYV(K,7)=STI(I)
        ENDDO
      ELSE
        IF(JTHE > 0) THEN
          DO I=1,NEL
            II = I +NFT
            K = IADP(1,II)
            FSKY(1,K)=-F11(I)
            FSKY(2,K)=-F21(I)
            FSKY(3,K)=-F31(I)
            FX1(I) = F11(I)
            FY1(I) = F21(I)
            FZ1(I) = F31(I)
            FSKY(7,K)=STI(I)
            FTHESKY(K) = FPHI(I,1)
            K = IADP(2,II)
            FSKY(1,K)=-F12(I)
            FSKY(2,K)=-F22(I)
            FSKY(3,K)=-F32(I)
            FX2(I) = F12(I)
            FY2(I) = F22(I)
            FZ2(I) = F32(I)
            FSKY(7,K)=STI(I)
            FTHESKY(K) = FPHI(I,2)
          ENDDO
        ELSE
          DO I=1,NEL
            II = I +NFT
            K = IADP(1,II)
            FSKY(1,K)=-F11(I)
            FSKY(2,K)=-F21(I)
            FSKY(3,K)=-F31(I)
            FX1(I) = F11(I)
            FY1(I) = F21(I)
            FZ1(I) = F31(I)
            FSKY(7,K)=STI(I)
            K = IADP(2,II)
            FSKY(1,K)=-F12(I)
            FSKY(2,K)=-F22(I)
            FSKY(3,K)=-F32(I)
            FX2(I) = F12(I)
            FY2(I) = F22(I)
            FZ2(I) = F32(I)
            FSKY(7,K)=STI(I)
          ENDDO        
        
        ENDIF  
      ENDIF
C-------------------------
      RETURN
      END
