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
!||    s4cumu3p        ../engine/source/elements/solid/solide4/s4cumu3p.F
!||--- called by ------------------------------------------------------
!||    multi_fvm2fem   ../engine/source/multifluid/multi_fvm2fem.F
!||    s4forc3         ../engine/source/elements/solid/solide4/s4forc3.F
!||====================================================================
      SUBROUTINE S4CUMU3P(
     1   OFFG,    STI,     FSKY,    FSKYV,
     2   IADS,    F11,     F21,     F31,
     3   F12,     F22,     F32,     F13,
     4   F23,     F33,     F14,     F24,
     5   F34,     THEM,    FTHESKY, CONDNSKY,
     6   CONDE,   NEL,     NFT,     JTHE    ,    
     7   NODADT_THERM)
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
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: NODADT_THERM
      my_real
     .   OFFG(*),FSKYV(LSKY,8),FSKY(8,LSKY),STI(*),
     .   F11(*),F21(*),F31(*),F12(*),F22(*),F32(*),
     .   F13(*),F23(*),F33(*),F14(*),F24(*),F34(*),
     .   THEM(MVSIZ,4),FTHESKY(*),CONDNSKY(*),CONDE(*)
      INTEGER IADS(8,*)
CMasParINCLUDE 'scumu3.intmap.inc'
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K,J
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=1,NEL
Cf small3b        IF(OFF(I)<1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO I=1,NEL
         IF(OFFG(I)<ZERO)THEN
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
           STI(I)=ZERO
         ENDIF
        ENDDO
      ENDIF
      IF(JTHE < 0 ) THEN
       IF(OFF_L<=ZERO)THEN
        DO J=1,4
         DO I=1,NEL
          IF(OFFG(I)<=ZERO)THEN
             THEM(I,J)=ZERO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF(NODADT_THERM == 1) THEN
        IF(OFF_L<ZERO)THEN
         DO I=1,NEL
          IF(OFFG(I)<ZERO)THEN
             CONDE(I)=ZERO
          ENDIF
         ENDDO
        ENDIF
       ENDIF  
      ENDIF
C
C     because 2*Mnodal = 2*Melement/4
      DO I=1,NEL
        STI(I)=HALF*STI(I)
      END DO
      IF(NODADT_THERM == 1 ) THEN  
        DO I=1,NEL
           CONDE(I)=FOURTH*CONDE(I)
        END DO
      ENDIF
C
      IF(IVECTOR==1) THEN
#include "vectorize.inc"
        DO I=1,NEL
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
          K = IADS(3,II)
          FSKYV(K,1)=F12(I)
          FSKYV(K,2)=F22(I)
          FSKYV(K,3)=F32(I)
          FSKYV(K,4)=ZERO
          FSKYV(K,5)=ZERO
          FSKYV(K,6)=ZERO 
          FSKYV(K,7)=STI(I)
C
          K = IADS(6,II)
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
        ENDDO
      ELSE
        DO I=1,NEL
          II=I+NFT
          K = IADS(1,II)
          FSKY(1,K)=F11(I)
          FSKY(2,K)=F21(I)
          FSKY(3,K)=F31(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(3,II)
          FSKY(1,K)=F12(I)
          FSKY(2,K)=F22(I)
          FSKY(3,K)=F32(I)
          FSKY(7,K)=STI(I)
C
          K = IADS(6,II)
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
        ENDDO
      ENDIF
C     
      IF(JTHE < 0) THEN
        DO I=1,NEL
          II=I+NFT
          K = IADS(1,II)
          FTHESKY(K)=THEM(I,1) 
C
          K = IADS(3,II)
          FTHESKY(K)=THEM(I,2)
C
          K = IADS(6,II)
          FTHESKY(K)=THEM(I,3)
C
          K = IADS(5,II)
          FTHESKY(K)=THEM(I,4)
C
        ENDDO
        IF(NODADT_THERM == 1 ) THEN  
         DO I=1,NEL
          II=I+NFT
          K = IADS(1,II)
          CONDNSKY(K)=CONDE(I) 
C
          K = IADS(3,II)
          CONDNSKY(K)=CONDE(I)
C
          K = IADS(6,II)
          CONDNSKY(K)=CONDE(I)
C
          K = IADS(5,II)
          CONDNSKY(K)=CONDE(I)
C
         ENDDO
        ENDIF
      ENDIF
C      
      RETURN
      END
