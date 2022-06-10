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
Chd|  FCUMU3                        source/elements/solid/solide/fcumu3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FCUMU3(OFFG,F,NC1, NC2, NC3, NC4,
     2   NC5, NC6, NC7, NC8,STIFN,STI,
     3   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     4   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     5   NVC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*),
     .   NC8(*),NVC
C     REAL
      my_real
     .   OFFG(*),F(3,*),STIFN(*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J
      my_real
     .   OFF_L
      INTEGER NVC1,NVC2,NVC3,NVC4,NVC5,NVC6,NVC7,NVC8
C-----------------------------------------------
      OFF_L = 0.
      DO I=LFT,LLT
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L.LT.ZERO)THEN
        DO I=LFT,LLT
         IF(OFFG(I) >= ZERO)CYCLE
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
           F17(I)=ZERO
           F27(I)=ZERO
           F37(I)=ZERO
           F18(I)=ZERO
           F28(I)=ZERO
           F38(I)=ZERO
           STI(I)=ZERO
        ENDDO
      ENDIF
C
      NVC1 = NVC / 128
      NVC2 =(NVC-NVC1*128) / 64
      NVC3 =(NVC-NVC1*128-NVC2*64) / 32
      NVC4 =(NVC-NVC1*128-NVC2*64-NVC3*32)/16
      NVC5 =(NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16)/8
      NVC6 =(NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8)/4
      NVC7 =(NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4)/2
      NVC8 = NVC-NVC1*128-NVC2*64-NVC3*32-NVC4*16-NVC5*8-NVC6*4-NVC7*2
C
C     because 2*Mnodal = 2*Melement/8
      DO I=LFT,LLT
        STI(I)=FOURTH*STI(I)
      END DO
C
      IF(NVC1 == 0)THEN
#include "vectorize.inc"
        DO I=LFT,LLT
         F(1,NC1(I))=F(1,NC1(I))+F11(I)
         F(2,NC1(I))=F(2,NC1(I))+F21(I)
         F(3,NC1(I))=F(3,NC1(I))+F31(I)
cc         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         F(1,NC1(I))=F(1,NC1(I))+F11(I)
         F(2,NC1(I))=F(2,NC1(I))+F21(I)
         F(3,NC1(I))=F(3,NC1(I))+F31(I)
cc         STIFN(NC1(I))=STIFN(NC1(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC2 == 0)THEN
#include "vectorize.inc"
        DO I=LFT,LLT
         F(1,NC2(I))=F(1,NC2(I))+F12(I)
         F(2,NC2(I))=F(2,NC2(I))+F22(I)
         F(3,NC2(I))=F(3,NC2(I))+F32(I)
cc         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         F(1,NC2(I))=F(1,NC2(I))+F12(I)
         F(2,NC2(I))=F(2,NC2(I))+F22(I)
         F(3,NC2(I))=F(3,NC2(I))+F32(I)
cc         STIFN(NC2(I))=STIFN(NC2(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC3 == 0)THEN
#include "vectorize.inc"
        DO I=LFT,LLT
         F(1,NC3(I))=F(1,NC3(I))+F13(I)
         F(2,NC3(I))=F(2,NC3(I))+F23(I)
         F(3,NC3(I))=F(3,NC3(I))+F33(I)
cc         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         F(1,NC3(I))=F(1,NC3(I))+F13(I)
         F(2,NC3(I))=F(2,NC3(I))+F23(I)
         F(3,NC3(I))=F(3,NC3(I))+F33(I)
cc         STIFN(NC3(I))=STIFN(NC3(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC4 == 0)THEN
#include "vectorize.inc"
        DO I=LFT,LLT
         F(1,NC4(I))=F(1,NC4(I))+F14(I)
         F(2,NC4(I))=F(2,NC4(I))+F24(I)
         F(3,NC4(I))=F(3,NC4(I))+F34(I)
cc         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         F(1,NC4(I))=F(1,NC4(I))+F14(I)
         F(2,NC4(I))=F(2,NC4(I))+F24(I)
         F(3,NC4(I))=F(3,NC4(I))+F34(I)
cc         STIFN(NC4(I))=STIFN(NC4(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC5 == 0)THEN
#include "vectorize.inc"
        DO I=LFT,LLT
         F(1,NC5(I))=F(1,NC5(I))+F15(I)
         F(2,NC5(I))=F(2,NC5(I))+F25(I)
         F(3,NC5(I))=F(3,NC5(I))+F35(I)
cc         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         F(1,NC5(I))=F(1,NC5(I))+F15(I)
         F(2,NC5(I))=F(2,NC5(I))+F25(I)
         F(3,NC5(I))=F(3,NC5(I))+F35(I)
cc         STIFN(NC5(I))=STIFN(NC5(I))+STI(I)
       ENDDO
       ENDIF
       IF(NVC6 == 0)THEN
#include "vectorize.inc"
        DO I=LFT,LLT
         F(1,NC6(I))=F(1,NC6(I))+F16(I)
         F(2,NC6(I))=F(2,NC6(I))+F26(I)
         F(3,NC6(I))=F(3,NC6(I))+F36(I)
cc         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         F(1,NC6(I))=F(1,NC6(I))+F16(I)
         F(2,NC6(I))=F(2,NC6(I))+F26(I)
         F(3,NC6(I))=F(3,NC6(I))+F36(I)
cc         STIFN(NC6(I))=STIFN(NC6(I))+STI(I)
        ENDDO
       ENDIF
       IF(NVC7 == 0)THEN
#include "vectorize.inc"
        DO I=LFT,LLT
         F(1,NC7(I))=F(1,NC7(I))+F17(I)
         F(2,NC7(I))=F(2,NC7(I))+F27(I)
         F(3,NC7(I))=F(3,NC7(I))+F37(I)
cc         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
        ENDDO
       ELSE
        DO I=LFT,LLT
         F(1,NC7(I))=F(1,NC7(I))+F17(I)
         F(2,NC7(I))=F(2,NC7(I))+F27(I)
         F(3,NC7(I))=F(3,NC7(I))+F37(I)
cc         STIFN(NC7(I))=STIFN(NC7(I))+STI(I)
        ENDDO
       ENDIF
      IF(NVC8 == 0)THEN
#include "vectorize.inc"
       DO I=LFT,LLT
         F(1,NC8(I))=F(1,NC8(I))+F18(I)
         F(2,NC8(I))=F(2,NC8(I))+F28(I)
         F(3,NC8(I))=F(3,NC8(I))+F38(I)
cc         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
       ENDDO
      ELSE
       DO I=LFT,LLT
         F(1,NC8(I))=F(1,NC8(I))+F18(I)
         F(2,NC8(I))=F(2,NC8(I))+F28(I)
         F(3,NC8(I))=F(3,NC8(I))+F38(I)
cc         STIFN(NC8(I))=STIFN(NC8(I))+STI(I)
        ENDDO
      ENDIF  
C
      RETURN
      END
Chd|====================================================================
Chd|  FCUMU3P                       source/elements/solid/solide/fcumu3.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FCUMU3P(OFFG,STI,FSKY,IADS,
     2   F11,F21,F31,F12,F22,F32,F13,F23,F33,F14,F24,F34,
     3   F15,F25,F35,F16,F26,F36,F17,F27,F37,F18,F28,F38,
     4   NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8)
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
#include      "vect01_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
C 
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*),
     .   NC8(*)
      my_real
     .   OFFG(*),FSKY(3,*),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .   F17(*),F27(*),F37(*),F18(*),F28(*),F38(*)
C
      INTEGER IADS(8,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K, J
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=LFT,LLT
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L < ZERO)THEN
        DO I=LFT,LLT
         IF(OFFG(I) >= ZERO)CYCLE
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
           F17(I)=ZERO
           F27(I)=ZERO
           F37(I)=ZERO
           F18(I)=ZERO
           F28(I)=ZERO
           F38(I)=ZERO
           STI(I)=ZERO
        ENDDO
      ENDIF
C
C     because 2*Mnodal = 2*Melement/8
      DO I=LFT,LLT
        STI(I)=FOURTH*STI(I)
      END DO
C
cc       RETURN
C
        DO I=LFT,LLT
          II=I + NFT
          K = IADS(1,II)
c          F21(I)=F11(I)
c          F11(I) = ZERO
                      
          FSKY(1,K)=F11(I)
          FSKY(2,K)=F21(I)
          FSKY(3,K)=F31(I) 
C
          K = IADS(7,II)
          FSKY(1,K)=F17(I)
          FSKY(2,K)=F27(I)
          FSKY(3,K)=F37(I)       
C
          K = IADS(8,II)
          FSKY(1,K)=F18(I)
          FSKY(2,K)=F28(I)
          FSKY(3,K)=F38(I)    
C
          K = IADS(3,II)
          FSKY(1,K)=F13(I)
          FSKY(2,K)=F23(I)
          FSKY(3,K)=F33(I)    
C
          K = IADS(5,II)
          FSKY(1,K)=F15(I)
          FSKY(2,K)=F25(I)
          FSKY(3,K)=F35(I)    
C
          K = IADS(4,II)
          FSKY(1,K)=F14(I)
          FSKY(2,K)=F24(I)
          FSKY(3,K)=F34(I)    
C
          K = IADS(6,II)
          FSKY(1,K)=F16(I)
          FSKY(2,K)=F26(I)
          FSKY(3,K)=F36(I) 
C          
          K = IADS(2,II)
          FSKY(1,K) = F12(I)
          FSKY(2,K)=F22(I)
          FSKY(3,K)=F32(I)   
        ENDDO
        
      RETURN
      END
