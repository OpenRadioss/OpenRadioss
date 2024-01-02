Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  QCUMU2P                       source/elements/solid_2d/quad/qcumu2p.F
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QCUMU2P(
     1   F11,     F12,     F21,     F22,
     2   AX1,     AX2,     T11,     T12,
     3   T13,     T14,     T21,     T22,
     4   T23,     T24,     FSKY,    FSKYV,
     5   IADQ,    STI,     NEL,     NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER  IADQ(4,*)
C     REAL
      my_real
     .   FSKYV(LSKY,8),FSKY(8,LSKY),STI(*)
C     REAL
      my_real
     .   F11(*), F12(*), F21(*), F22(*), AX1(*), AX2(*),
     .   T11(*), T12(*), T13(*), T14(*), T21(*), T22(*), T23(*), T24(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, II, K
C-------------------------
C     ASSEMBLE
C-------------------------
C
      IF(IVECTOR==1) THEN
        DO I=1,NEL
C
          II=I+NFT
          K = IADQ(1,II)
          FSKYV(K,2)=-F11(I)-AX1(I)-T11(I)
          FSKYV(K,3)=-F21(I)+AX2(I)-T21(I)
          FSKYV(K,7)=STI(I)
C
          K = IADQ(2,II)
          FSKYV(K,2)=-F12(I)-AX1(I)-T12(I)
          FSKYV(K,3)=-F22(I)+AX2(I)-T22(I)
          FSKYV(K,7)=STI(I)
C
          K = IADQ(3,II)
          FSKYV(K,2)=+F11(I)-AX1(I)-T13(I)
          FSKYV(K,3)=+F21(I)+AX2(I)-T23(I)
          FSKYV(K,7)=STI(I)
C
          K = IADQ(4,II)
          FSKYV(K,2)=+F12(I)-AX1(I)-T14(I)
          FSKYV(K,3)=+F22(I)+AX2(I)-T24(I)
          FSKYV(K,7)=STI(I)
C
        ENDDO
C
      ELSE
        DO I=1,NEL
C
          II=I+NFT
          K = IADQ(1,II)
          FSKY(2,K)=-F11(I)-AX1(I)-T11(I)
          FSKY(3,K)=-F21(I)+AX2(I)-T21(I)
          FSKY(7,K) = STI(I)
C
          K = IADQ(2,II)
          FSKY(2,K)=-F12(I)-AX1(I)-T12(I)
          FSKY(3,K)=-F22(I)+AX2(I)-T22(I)
          FSKY(7,K) = STI(I)
C
          K = IADQ(3,II)
          FSKY(2,K)=+F11(I)-AX1(I)-T13(I)
          FSKY(3,K)=+F21(I)+AX2(I)-T23(I)
          FSKY(7,K) = STI(I)
C
          K = IADQ(4,II)
          FSKY(2,K)=+F12(I)-AX1(I)-T14(I)
          FSKY(3,K)=+F22(I)+AX2(I)-T24(I)
          FSKY(7,K) = STI(I)
C
        ENDDO
C
      ENDIF
C
      RETURN
      END
