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
!||    rcum33p   ../engine/source/elements/joint/rcum33p.F
!||--- called by ------------------------------------------------------
!||    rgjoint   ../engine/source/elements/joint/rgjoint.F
!||====================================================================
      SUBROUTINE RCUM33P(
     1   JFT,     JLT,     XL,      FSKY,
     2   FSKYV,   FORX,    FORY,    FORZ,
     3   XMOM,    YMOM,    ZMOM,    STI,
     4   STIR,    FX1,     FX2,     FY1,
     5   FY2,     FZ1,     FZ2,     MX1,
     6   MX2,     MY1,     MY2,     MZ1,
     7   MZ2,     IADR,    NUVAR,   UVAR,
     8   NFT)
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER JFT,JLT,IADR(3,*),NUVAR
      my_real FORX(*), FORY(*), FORZ(*), XMOM(*), YMOM(*), ZMOM(*),
     .        STI(3,*),STIR(3,*),FSKY(8,LSKY),FSKYV(LSKY,8),
     .        FX1(*),FX2(*),FY1(*),FY2(*),FZ1(*),FZ2(*),
     .        MX1(*),MX2(*),MY1(*),MY2(*),MZ1(*),MZ2(*),UVAR(NUVAR,*)
      DOUBLE PRECISION XL(MVSIZ,3)     
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, N,J
      my_real F11(MVSIZ), F21(MVSIZ), F31(MVSIZ),
     .        M11(MVSIZ), M21(MVSIZ), M31(MVSIZ),
     .        M12(MVSIZ), M22(MVSIZ), M32(MVSIZ),
     .        XMOM1, XMOM2, YMOM1, YMOM2, ZMOM1, ZMOM2, M1,M2,M3,
     .        EX(9), XX
C=======================================================================
C-------------------------
C     FORCES
C-------------------------
C
      DO I=JFT,JLT
        DO J=1,9
          EX(J) = UVAR(21+J,I)
        END DO      
        F11(I)=EX(1)*FORX(I)+EX(4)*FORY(I)+EX(7)*FORZ(I)
        F21(I)=EX(2)*FORX(I)+EX(5)*FORY(I)+EX(8)*FORZ(I)
        F31(I)=EX(3)*FORX(I)+EX(6)*FORY(I)+EX(9)*FORZ(I)
      ENDDO
C
C----  Assembly
C
      IF(IVECTOR==1) THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          II = I+NFT
          N = IADR(1,II)
          FSKYV(N,1)=F11(I)
          FSKYV(N,2)=F21(I)
          FSKYV(N,3)=F31(I)
          FX1(I) = -F11(I)
          FY1(I) = -F21(I)
          FZ1(I) = -F31(I)
          FSKYV(N,7)=STI(1,I)*TWO
          N = IADR(2,II)
          FSKYV(N,1)=-F11(I)
          FSKYV(N,2)=-F21(I)
          FSKYV(N,3)=-F31(I)
          FX2(I) = F11(I)
          FY2(I) = F21(I)
          FZ2(I) = F31(I)
          FSKYV(N,7)=STI(2,I)*TWO
        ENDDO
      ELSE
        DO I=JFT,JLT
          II = I+NFT
          N = IADR(1,II)
          FSKY(1,N)=F11(I)
          FSKY(2,N)=F21(I)
          FSKY(3,N)=F31(I)
          FX1(I) = -F11(I)
          FY1(I) = -F21(I)
          FZ1(I) = -F31(I)
          FSKY(7,N)=STI(1,I)*TWO
          N = IADR(2,II)
          FSKY(1,N)=-F11(I)
          FSKY(2,N)=-F21(I)
          FSKY(3,N)=-F31(I)
          FX2(I) = F11(I)
          FY2(I) = F21(I)
          FZ2(I) = F31(I)
          FSKY(7,N)=STI(2,I)*TWO
        ENDDO
      ENDIF
C
C-------------------------
C     momentum
C-------------------------
C
      DO I=JFT,JLT
        DO J=1,9
          EX(J) = UVAR(21+J,I)
        END DO
C
        M1 = HALF*(XL(I,2)*FORZ(I)-XL(I,3)*FORY(I))
        M2 = HALF*(XL(I,3)*FORX(I)-XL(I,1)*FORZ(I))
        M3 = HALF*(XL(I,1)*FORY(I)-XL(I,2)*FORX(I))
C
        XMOM1 = XMOM(I) + M1
        YMOM1 = YMOM(I) + M2
        ZMOM1 = ZMOM(I) + M3
        M11(I)=EX(1)*XMOM1+EX(4)*YMOM1+EX(7)*ZMOM1
        M21(I)=EX(2)*XMOM1+EX(5)*YMOM1+EX(8)*ZMOM1
        M31(I)=EX(3)*XMOM1+EX(6)*YMOM1+EX(9)*ZMOM1
C
        XMOM2 = XMOM(I) - M1
        YMOM2 = YMOM(I) - M2
        ZMOM2 = ZMOM(I) - M3
        M12(I)=EX(1)*XMOM2+EX(4)*YMOM2+EX(7)*ZMOM2
        M22(I)=EX(2)*XMOM2+EX(5)*YMOM2+EX(8)*ZMOM2
        M32(I)=EX(3)*XMOM2+EX(6)*YMOM2+EX(9)*ZMOM2
C
        XX = XL(I,1)*XL(I,1) + XL(I,2)*XL(I,2) + XL(I,3)*XL(I,3)
        STIR(1,I) = STIR(1,I) + STI(2,I)*XX
        STIR(2,I) = STIR(2,I) + STI(1,I)*XX
      ENDDO
C
C----   Assemble
C
      IF(IVECTOR==1) THEN
#include "vectorize.inc"
        DO I=JFT,JLT
          II = I+NFT
          N = IADR(1,II)
          FSKYV(N,4)=M11(I)
          FSKYV(N,5)=M21(I)
          FSKYV(N,6)=M31(I)
          MX1(I) = -M11(I)
          MY1(I) = -M21(I)
          MZ1(I) = -M31(I)
          FSKYV(N,8)=STIR(1,I)
          N = IADR(2,II)
          FSKYV(N,4)=-M12(I)
          FSKYV(N,5)=-M22(I)
          FSKYV(N,6)=-M32(I)
          MX2(I) = M12(I)
          MY2(I) = M22(I)
          MZ2(I) = M32(I)
          FSKYV(N,8)=STIR(2,I)
        ENDDO
      ELSE
        DO I=JFT,JLT
          II = I+NFT
          N = IADR(1,II)
          FSKY(4,N)=M11(I)
          FSKY(5,N)=M21(I)
          FSKY(6,N)=M31(I)
          MX1(I) = -M11(I)
          MY1(I) = -M21(I)
          MZ1(I) = -M31(I)
          FSKY(8,N)=STIR(1,I)
          N = IADR(2,II)
          FSKY(4,N)=-M12(I)
          FSKY(5,N)=-M22(I)
          FSKY(6,N)=-M32(I)
          MX2(I) = M12(I)
          MY2(I) = M22(I)
          MZ2(I) = M32(I)
          FSKY(8,N)=STIR(2,I)
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
