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
Cqz nouvelle routine base sur s4cumu3.F------
Chd|====================================================================
Chd|  S6CUMU3                       source/elements/thickshell/solide6c/s6cumu3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CUMU3(OFFG,E,NC1, NC2, NC3, NC4,
     .   NC5, NC6,STIFN,STI,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36)
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
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*)
C     REAL ou REAL*8
      my_real
     .   OFFG(*),E(3,*),STIFN(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
C-----------------------------------------------
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=LFT,LLT
Cf small3b        IF(OFF(I).LT.1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L.LT.ZERO)THEN
        DO I=LFT,LLT
         IF(OFFG(I).LT.ZERO)THEN
           F11(I)=ZERO
           F21(I)=ZERO
           F31(I)=ZERO
           F12(I)=ZERO
           F22(I)=ZERO
           F32(I)=ZERO
           F13(I)=ZERO
           F23(I)=ZERO
           F33(I)=ZERO
           F14(I)=ZERO
           F24(I)=ZERO
           F34(I)=ZERO
           F15(I)=ZERO
           F25(I)=ZERO
           F35(I)=ZERO
           F16(I)=ZERO
           F26(I)=ZERO
           F36(I)=ZERO
           STI(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C
C     because 2*Mnodal = 2*Melement/6
      DO I=LFT,LLT
        STI(I)=TIERS*STI(I)
      END DO
C
      DO 100 I=LFT,LLT
C
      E(1,NC1(I))=E(1,NC1(I))+F11(I)
      E(2,NC1(I))=E(2,NC1(I))+F21(I)
      E(3,NC1(I))=E(3,NC1(I))+F31(I)
      STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
C
      E(1,NC2(I))=E(1,NC2(I))+F12(I)
      E(2,NC2(I))=E(2,NC2(I))+F22(I)
      E(3,NC2(I))=E(3,NC2(I))+F32(I)
      STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
C
      E(1,NC3(I))=E(1,NC3(I))+F13(I)
      E(2,NC3(I))=E(2,NC3(I))+F23(I)
      E(3,NC3(I))=E(3,NC3(I))+F33(I)
      STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
C
      E(1,NC5(I))=E(1,NC5(I))+F15(I)
      E(2,NC5(I))=E(2,NC5(I))+F25(I)
      E(3,NC5(I))=E(3,NC5(I))+F35(I)
      STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
C
      E(1,NC4(I))=E(1,NC4(I))+F14(I)
      E(2,NC4(I))=E(2,NC4(I))+F24(I)
      E(3,NC4(I))=E(3,NC4(I))+F34(I)
      STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
C
      E(1,NC6(I))=E(1,NC6(I))+F16(I)
      E(2,NC6(I))=E(2,NC6(I))+F26(I)
      E(3,NC6(I))=E(3,NC6(I))+F36(I)
      STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
C
  100 CONTINUE
      RETURN
      END
Chd|====================================================================
Chd|  S6CUMU3P                      source/elements/thickshell/solide6c/s6cumu3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CUMU3P(OFFG,STI,FSKY,FSKYV,IADS,
     .   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     .   F15,F25,F35,F16,F26,F36)
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
C     REAL ou REAL*8
      my_real
     .   OFFG(*),FSKYV(LSKY,8),FSKY(8,LSKY),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*)
      INTEGER IADS(8,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C ds 41i 001 13/6/00  +1
C     INTEGER I, II, K
      INTEGER I, II, K, J
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=LFT,LLT
Cf small3b        IF(OFF(I).LT.1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L.LT.ZERO)THEN
        DO I=LFT,LLT
         IF(OFFG(I).LT.ZERO)THEN
           F11(I)=ZERO
           F21(I)=ZERO
           F31(I)=ZERO
           F12(I)=ZERO
           F22(I)=ZERO
           F32(I)=ZERO
           F13(I)=ZERO
           F23(I)=ZERO
           F33(I)=ZERO
           F14(I)=ZERO
           F24(I)=ZERO
           F34(I)=ZERO
           F15(I)=ZERO
           F25(I)=ZERO
           F35(I)=ZERO
           F16(I)=ZERO
           F26(I)=ZERO
           F36(I)=ZERO
           STI(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
C-----------------------------------------------
C
C     because 2*Mnodal = 2*Melement/6
      DO I=LFT,LLT
        STI(I)=TIERS*STI(I)
      END DO
C
      IF(IVECTOR.EQ.1) THEN
#include "vectorize.inc"
        DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKYV(K,1)=F11(I)
          FSKYV(K,2)=F21(I)
          FSKYV(K,3)=F31(I)
          FSKYV(K,4)=ZERO
          FSKYV(K,5)=ZERO
          FSKYV(K,6)=ZERO 
          FSKYV(K,7)=STI(I)
C
          K = IADS(2,II)
          FSKYV(K,1)=F12(I)
          FSKYV(K,2)=F22(I)
          FSKYV(K,3)=F32(I)
          FSKYV(K,4)=ZERO
          FSKYV(K,5)=ZERO
          FSKYV(K,6)=ZERO 
          FSKYV(K,7)=STI(I)
C
          K = IADS(3,II)
          FSKYV(K,1)=F13(I)
          FSKYV(K,2)=F23(I)
          FSKYV(K,3)=F33(I)
          FSKYV(K,4)=ZERO
          FSKYV(K,5)=ZERO
          FSKYV(K,6)=ZERO
          FSKYV(K,7)=STI(I)
C
          K = IADS(5,II)
          FSKYV(K,1)=F14(I)
          FSKYV(K,2)=F24(I)
          FSKYV(K,3)=F34(I)
          FSKYV(K,4)=ZERO
          FSKYV(K,5)=ZERO
          FSKYV(K,6)=ZERO 
          FSKYV(K,7)=STI(I)
C
          K = IADS(6,II)
          FSKYV(K,1)=F15(I)
          FSKYV(K,2)=F25(I)
          FSKYV(K,3)=F35(I)
          FSKYV(K,4)=ZERO
          FSKYV(K,5)=ZERO
          FSKYV(K,6)=ZERO 
          FSKYV(K,7)=STI(I)
C
          K = IADS(7,II)
          FSKYV(K,1)=F16(I)
          FSKYV(K,2)=F26(I)
          FSKYV(K,3)=F36(I)
          FSKYV(K,4)=ZERO
          FSKYV(K,5)=ZERO
          FSKYV(K,6)=ZERO 
          FSKYV(K,7)=STI(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          II=I+NFT
          K = IADS(1,II)
          FSKY(1,K)=F11(I)
          FSKY(2,K)=F21(I)
          FSKY(3,K)=F31(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(2,II)
          FSKY(1,K)=F12(I)
          FSKY(2,K)=F22(I)
          FSKY(3,K)=F32(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(3,II)
          FSKY(1,K)=F13(I)
          FSKY(2,K)=F23(I)
          FSKY(3,K)=F33(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(5,II)
          FSKY(1,K)=F14(I)
          FSKY(2,K)=F24(I)
          FSKY(3,K)=F34(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(6,II)
          FSKY(1,K)=F15(I)
          FSKY(2,K)=F25(I)
          FSKY(3,K)=F35(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(7,II)
          FSKY(1,K)=F16(I)
          FSKY(2,K)=F26(I)
          FSKY(3,K)=F36(I)
          FSKY(7,K)=STI(I)
        ENDDO
      ENDIF
C
      RETURN
      END
