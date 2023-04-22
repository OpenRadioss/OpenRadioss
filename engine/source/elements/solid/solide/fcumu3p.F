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
Chd|  FCUMU3P                       source/elements/solid/solide/fcumu3p.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FCUMU3P(
     1   OFFG,    STI,     FSKY,    IADS,
     2   F11,     F21,     F31,     F12,
     3   F22,     F32,     F13,     F23,
     4   F33,     F14,     F24,     F34,
     5   F15,     F25,     F35,     F16,
     6   F26,     F36,     F17,     F27,
     7   F37,     F18,     F28,     F38,
     8   NC1,     NC2,     NC3,     NC4,
     9   NC5,     NC6,     NC7,     NC8,
     A   NEL,     NFT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER NC1(*), NC2(*), NC3(*), NC4(*), NC5(*), NC6(*), NC7(*), NC8(*)
      my_real  OFFG(*),FSKY(3,*),STI(*),
     .         F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .         F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .         F15(*),F25(*),F35(*),F16(*),F26(*),F36(*),
     .         F17(*),F27(*),F37(*),F18(*),F28(*),F38(*)
C
      INTEGER IADS(8,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K, J
      my_real OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=1,NEL
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L < ZERO)THEN
        DO I=1,NEL
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
      DO I=1,NEL
        STI(I)=FOURTH*STI(I)
      END DO
C
cc       RETURN
C
        DO I=1,NEL
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
