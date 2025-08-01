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
!||    pmcum3p   ../engine/source/elements/beam/pmcum3p.F
!||--- called by ------------------------------------------------------
!||    pforc3    ../engine/source/elements/beam/pforc3.F
!||====================================================================
      SUBROUTINE PMCUM3P(
     1   STIR,    FSKY,    FSKYV,   IADP,
     2   MX1,     MX2,     MY1,     MY2,
     3   MZ1,     MZ2,     NC1,     NC2,
     4   NC3,     E1X,     E1Y,     E1Z,
     5   E2X,     E2Y,     E2Z,     E3X,
     6   E3Y,     E3Z,     M11,     M12,
     7   M21,     M22,     M31,     M32,
     8   NEL,     NFT)
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
      INTEGER NC1(MVSIZ),NC2(MVSIZ),NC3(MVSIZ)
      my_real
     .   FSKYV(LSKY,8),FSKY(8,LSKY),STIR(*),
     .   MX1(MVSIZ),MY1(MVSIZ),MZ1(MVSIZ),
     .   MX2(MVSIZ),MY2(MVSIZ),MZ2(MVSIZ),
     .   E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),
     .   E2X(MVSIZ),E2Y(MVSIZ),E2Z(MVSIZ),
     .   E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ),
     .   M11(MVSIZ),M12(MVSIZ),M21(MVSIZ),
     .   M22(MVSIZ),M31(MVSIZ),M32(MVSIZ)
      INTEGER IADP(2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, N,J
      my_real
     .   G11(MVSIZ),G21(MVSIZ),G31(MVSIZ),
     .   G12(MVSIZ),G22(MVSIZ),G32(MVSIZ)
C-----------------------------------------------
      DO I=1,NEL
       G11(I)=M11(I)
       G21(I)=M21(I)
       G31(I)=M31(I)
       G12(I)=M12(I)
       G22(I)=M22(I)
       G32(I)=M32(I)
      ENDDO
C
      DO I=1,NEL
       M11(I)=E1X(I)*G11(I)+E2X(I)*G21(I)+E3X(I)*G31(I)
       M21(I)=E1Y(I)*G11(I)+E2Y(I)*G21(I)+E3Y(I)*G31(I)
       M31(I)=E1Z(I)*G11(I)+E2Z(I)*G21(I)+E3Z(I)*G31(I)
       M12(I)=E1X(I)*G12(I)+E2X(I)*G22(I)+E3X(I)*G32(I)
       M22(I)=E1Y(I)*G12(I)+E2Y(I)*G22(I)+E3Y(I)*G32(I)
       M32(I)=E1Z(I)*G12(I)+E2Z(I)*G22(I)+E3Z(I)*G32(I)
      ENDDO
C-------------------------
C     ASSEMBLAGE
C-------------------------
      IF (IVECTOR == 1) THEN
#include "vectorize.inc"
        DO I=1,NEL
          II = I + NFT
          N = IADP(1,II)
          FSKYV(N,4)=-M11(I)
          FSKYV(N,5)=-M21(I)
          FSKYV(N,6)=-M31(I)
          MX1(I) = M11(I)
          MY1(I) = M21(I)
          MZ1(I) = M31(I)
          FSKYV(N,8)=STIR(I)
          N = IADP(2,II)
          FSKYV(N,4)=-M12(I)
          FSKYV(N,5)=-M22(I)
          FSKYV(N,6)=-M32(I)
          MX2(I) = M12(I)
          MY2(I) = M22(I)
          MZ2(I) = M32(I)
          FSKYV(N,8)=STIR(I)
        ENDDO
      ELSE
        DO I=1,NEL
          II = I + NFT
          N = IADP(1,II)
          FSKY(4,N)=-M11(I)
          FSKY(5,N)=-M21(I)
          FSKY(6,N)=-M31(I)
          MX1(I) = M11(I)
          MY1(I) = M21(I)
          MZ1(I) = M31(I)
          FSKY(8,N)=STIR(I)
          N = IADP(2,II)
          FSKY(4,N)=-M12(I)
          FSKY(5,N)=-M22(I)
          FSKY(6,N)=-M32(I)
          MX2(I) = M12(I)
          MY2(I) = M22(I)
          MZ2(I) = M32(I)
          FSKY(8,N)=STIR(I)
        ENDDO
      ENDIF
C-----------------------------------------------
      RETURN
      END
