Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  QCUMU2                        source/elements/solid_2d/quad/qcumu2.F
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QCUMU2(E,
     .   F11, F12, F21, F22, AX1,AX2,
     .   T11, T12, T13, T14, T21, T22, T23,
     .   T24, NC1, NC2, NC3, NC4,STI,STIFN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   E(3,*),STIFN(*),STI(*)
C     REAL ou REAL*8
      my_real
     .   F11(*), F12(*), F21(*), F22(*), AX1(*), AX2(*),
     .   T11(*), T12(*), T13(*), T14(*), T21(*), T22(*), T23(*), T24(*)
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C-------------------------
C     ASSEMBLE
C-------------------------
C
      DO 100 I=LFT,LLT
C
      E(2,NC1(I))=E(2,NC1(I))-F11(I)-AX1(I)-T11(I)
      E(3,NC1(I))=E(3,NC1(I))-F21(I)+AX2(I)-T21(I)
      STIFN(NC1(I))=STIFN(NC1(I))+STI(I)

C
      E(2,NC2(I))=E(2,NC2(I))-F12(I)-AX1(I)-T12(I)
      E(3,NC2(I))=E(3,NC2(I))-F22(I)+AX2(I)-T22(I)
      STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
C
      E(2,NC3(I))=E(2,NC3(I))+F11(I)-AX1(I)-T13(I)
      E(3,NC3(I))=E(3,NC3(I))+F21(I)+AX2(I)-T23(I)
      STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
C
      E(2,NC4(I))=E(2,NC4(I))+F12(I)-AX1(I)-T14(I)
      E(3,NC4(I))=E(3,NC4(I))+F22(I)+AX2(I)-T24(I)
      STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
C
  100 CONTINUE
      RETURN
      END
Chd|====================================================================
Chd|  QCUMU2P                       source/elements/solid_2d/quad/qcumu2.F
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QCUMU2P(
     .   F11, F12, F21, F22, AX1, AX2,
     .   T11, T12, T13, T14, T21, T22, T23,
     .   T24,FSKY,FSKYV,IADQ,STI)
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
#include      "vect01_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IADQ(4,*)
C     REAL ou REAL*8
      my_real
     .   FSKYV(LSKY,8),FSKY(8,LSKY),STI(*)
C     REAL ou REAL*8
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
      IF(IVECTOR.EQ.1) THEN
        DO I=LFT,LLT
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
        DO I=LFT,LLT
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
