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
Chd|  Q4CUMU2                       source/elements/solid_2d/quad4/q4cumu2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4CUMU2(E,STIFN,
     2                   NC1,NC2,NC3,NC4,
     3                   FY1,FZ1,FY2,FZ2,
     4                   FY3,FZ3,FY4,FZ4,
     5                   STIM)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(*),NC2(*),NC3(*),NC4(*)
C     REAL
      my_real
     .    E(3,*),STIFN(*),STIM(*),
     .    FY1(*),FZ1(*),FY2(*),FZ2(*),
     .    FY3(*),FZ3(*),FY4(*),FZ4(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      DO I=LFT,LLT
        STIM(I)=FOURTH*STIM(I)
      END DO
C
      DO I=LFT,LLT
C
        E(2,NC1(I)) = E(2,NC1(I)) + FY1(I)
        E(3,NC1(I)) = E(3,NC1(I)) + FZ1(I)
        STIFN(NC1(I)) = STIFN(NC1(I)) + STIM(I)
C
        E(2,NC2(I)) = E(2,NC2(I)) + FY2(I)
        E(3,NC2(I)) = E(3,NC2(I)) + FZ2(I)
        STIFN(NC2(I)) = STIFN(NC2(I)) + STIM(I)
C
        E(2,NC3(I)) = E(2,NC3(I)) + FY3(I)
        E(3,NC3(I)) = E(3,NC3(I)) + FZ3(I)
        STIFN(NC3(I)) = STIFN(NC3(I)) + STIM(I)
C
        E(2,NC4(I)) = E(2,NC4(I)) + FY4(I)
        E(3,NC4(I)) = E(3,NC4(I)) + FZ4(I)
        STIFN(NC4(I)) = STIFN(NC4(I)) + STIM(I)
C
      ENDDO
C
      RETURN
      END
Chd|====================================================================
Chd|  Q4CUMU2P                      source/elements/solid_2d/quad4/q4cumu2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4CUMU2P(FSKY,FSKYV,IADQ,
     2                    FY1,FZ1,FY2,FZ2,
     3                    FY3,FZ3,FY4,FZ4,
     4                    STIM)
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
      INTEGER IADQ(4,*)
C     REAL
      my_real
     .    FSKY(8,LSKY), FSKYV(LSKY,8),
     .    FY1(*), FZ1(*), FY2(*), FZ2(*),
     .    FY3(*), FZ3(*), FY4(*), FZ4(*),
     .    STIM(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, II, K
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C
      DO I=LFT,LLT
        STIM(I)=FOURTH*STIM(I)
      END DO
C
      IF(IVECTOR==1) THEN
        DO I=LFT,LLT
          II = I + NFT
C
          K = IADQ(1,II)
          FSKYV(K,2) = FY1(I)
          FSKYV(K,3) = FZ1(I)
          FSKYV(K,7) = STIM(I)
C
          K = IADQ(2,II)
          FSKYV(K,2) = FY2(I)
          FSKYV(K,3) = FZ2(I)
          FSKYV(K,7) = STIM(I)
C
          K = IADQ(3,II)
          FSKYV(K,2) = FY3(I)
          FSKYV(K,3) = FZ3(I)
          FSKYV(K,7) = STIM(I)
C
          K = IADQ(4,II)
          FSKYV(K,2) = FY4(I)
          FSKYV(K,3) = FZ4(I)
          FSKYV(K,7) = STIM(I)
        ENDDO
      ELSE
        DO I=LFT,LLT
          II = I + NFT
C
          K = IADQ(1,II)
          FSKY(2,K) = FY1(I)
          FSKY(3,K) = FZ1(I)
          FSKY(7,K) = STIM(I)
C
          K = IADQ(2,II)
          FSKY(2,K) = FY2(I)
          FSKY(3,K) = FZ2(I)
          FSKY(7,K) = STIM(I)
C
          K = IADQ(3,II)
          FSKY(2,K) = FY3(I)
          FSKY(3,K) = FZ3(I)
          FSKY(7,K) = STIM(I)
C
          K = IADQ(4,II)
          FSKY(2,K) = FY4(I)
          FSKY(3,K) = FZ4(I)
          FSKY(7,K) = STIM(I)
        ENDDO
      ENDIF
C
      RETURN
      END
	  