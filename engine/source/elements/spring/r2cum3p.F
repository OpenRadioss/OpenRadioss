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
Chd|  R2CUM3P                       source/elements/spring/r2cum3.F
Chd|-- called by -----------
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE R2CUM3P(
     1   FORX,    FORY,    FORZ,    XMOM,
     2   YMOM,    ZMOM,    STI,     STIR,
     3   FSKY,    FSKYV,   IADR,    FX1,
     4   FX2,     FY1,     FY2,     FZ1,
     5   FZ2,     MX1,     MX2,     MY1,
     6   MY2,     MZ1,     MZ2,     GEO,
     7   X1,      Y1,      Z1,      X2,
     8   Y2,      Z2,      IEQUIL,  EXX,
     9   EYX,     EZX,     EXY,     EYY,
     A   EZY,     EXZ,     EYZ,     EZZ,
     B   NEL,     NFT)
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
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
C     REAL
      my_real
     .   FORX(*), FORY(*), FORZ(*), XMOM(*), YMOM(*),
     .   ZMOM(*),STI(3,*),STIR(3,*),FSKYV(LSKY,8), FSKY(8,LSKY),
     .   FX1(MVSIZ),FY1(MVSIZ),FZ1(MVSIZ),
     .   FX2(MVSIZ),FY2(MVSIZ),FZ2(MVSIZ),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .   GEO(NPROPG,*),X1(*),X2(*),Y1(*),Y2(*),
     .   Z1(*),Z2(*), 
     .   EXX(MVSIZ), EYX(MVSIZ), EZX(MVSIZ), EXY(MVSIZ),
     .   EYY(MVSIZ), EZY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ), EZZ(MVSIZ)
      INTEGER IADR(3,*),IEQUIL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, N, J
C     REAL
      my_real
     .   MMX, MMY, MMZ, XX
C-----------------------------------------------
C-------------------------
C     FORCES
C-------------------------
      DO I=1,NEL
        FX2(I)=EXX(I)*FORX(I)+EXY(I)*FORY(I)+EXZ(I)*FORZ(I)
        FY2(I)=EYX(I)*FORX(I)+EYY(I)*FORY(I)+EYZ(I)*FORZ(I)
        FZ2(I)=EZX(I)*FORX(I)+EZY(I)*FORY(I)+EZZ(I)*FORZ(I)
        FX1(I) = -FX2(I)
        FY1(I) = -FY2(I)
        FZ1(I) = -FZ2(I)
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKYV(N,1)=-FX1(I)
          FSKYV(N,2)=-FY1(I)
          FSKYV(N,3)=-FZ1(I)
          FSKYV(N,7)=STI(1,I)
          N = IADR(2,II)
          FSKYV(N,1)=-FX2(I)
          FSKYV(N,2)=-FY2(I)
          FSKYV(N,3)=-FZ2(I)
          FSKYV(N,7)=STI(2,I)
        ENDDO
      ELSE
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKY(1,N)=-FX1(I)
          FSKY(2,N)=-FY1(I)
          FSKY(3,N)=-FZ1(I)
          FSKY(7,N)=STI(1,I)
          N = IADR(2,II)
          FSKY(1,N)=-FX2(I)
          FSKY(2,N)=-FY2(I)
          FSKY(3,N)=-FZ2(I)
          FSKY(7,N)=STI(2,I)
        ENDDO
      ENDIF
C-------------------------
C     MOMENTS
C-------------------------
      DO I=1,NEL
        MX2(I)=EXX(I)*XMOM(I)+EXY(I)*YMOM(I)+EXZ(I)*ZMOM(I)
        MY2(I)=EYX(I)*XMOM(I)+EYY(I)*YMOM(I)+EYZ(I)*ZMOM(I)
        MZ2(I)=EZX(I)*XMOM(I)+EZY(I)*YMOM(I)+EZZ(I)*ZMOM(I)
        MX1(I) = -MX2(I)
        MY1(I) = -MY2(I)
        MZ1(I) = -MZ2(I)
      ENDDO
C
      DO I=1,NEL
        IF (IEQUIL(I) == 1) THEN
          MMX = HALF*((Y2(I)-Y1(I))*FZ2(I) - (Z2(I)-Z1(I))*FY2(I))
          MMY = HALF*((Z2(I)-Z1(I))*FX2(I) - (X2(I)-X1(I))*FZ2(I))
          MMZ = HALF*((X2(I)-X1(I))*FY2(I) - (Y2(I)-Y1(I))*FX2(I)) 
          MX1(I) = MX1(I) - MMX
          MY1(I) = MY1(I) - MMY
          MZ1(I) = MZ1(I) - MMZ
          MX2(I) = MX2(I) - MMX
          MY2(I) = MY2(I) - MMY
          MZ2(I) = MZ2(I) - MMZ
          XX = (X2(I)-X1(I))*(X2(I)-X1(I)) 
     .       + (Y2(I)-Y1(I))*(Y2(I)-Y1(I))
     .       + (Z2(I)-Z1(I))*(Z2(I)-Z1(I))
          STIR(1,I) = STIR(1,I) + STI(2,I)*XX
          STIR(2,I) = STIR(2,I) + STI(1,I)*XX
        ENDIF
      ENDDO
C-------------------------
C     ASSEMBLE
C-------------------------
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKYV(N,4)=-MX1(I)
          FSKYV(N,5)=-MY1(I)
          FSKYV(N,6)=-MZ1(I)
          FSKYV(N,8)=STIR(1,I)
          N = IADR(2,II)
          FSKYV(N,4)=-MX2(I)
          FSKYV(N,5)=-MY2(I)
          FSKYV(N,6)=-MZ2(I)
          FSKYV(N,8)=STIR(2,I)
        ENDDO
      ELSE
        DO I=1,NEL
          II = I+NFT
          N = IADR(1,II)
          FSKY(4,N)=-MX1(I)
          FSKY(5,N)=-MY1(I)
          FSKY(6,N)=-MZ1(I)
          FSKY(8,N)=STIR(1,I)
          N = IADR(2,II)
          FSKY(4,N)=-MX2(I)
          FSKY(5,N)=-MY2(I)
          FSKY(6,N)=-MZ2(I)
          FSKY(8,N)=STIR(2,I)
        ENDDO
      ENDIF
C---
      RETURN
      END
