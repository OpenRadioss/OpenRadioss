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
Chd|  S20CUMU3P                     source/elements/solid/solide20/s20cumu3p.F
Chd|-- called by -----------
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S20CUMU3P(
     1   OFFG,    STIG,    FSKY,    FSKYV,
     2   IADS,    FX,      FY,      FZ,
     3   IADSP,   NC,      IPERM1,  IPERM2,
     4   NPE,     THEM,    FTHESKY, CONDNSKY,
     5   CONDEG,  NEL,     NFT,     JTHE)
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
#include      "com04_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: JTHE
      INTEGER NPE
      INTEGER IADS(8,*),IADSP(NPE-8,*),NC(MVSIZ,NPE),
     .        IPERM1(NPE),IPERM2(NPE)
C     REAL
      my_real
     .   OFFG(*),FSKYV(LSKY,8),FSKY(8,LSKY),STIG(MVSIZ,NPE),
     .   FX(MVSIZ,NPE), FY(MVSIZ,NPE), FZ(MVSIZ,NPE),THEM(MVSIZ,NPE),
     .   FTHESKY(*),CONDNSKY(*),CONDEG(MVSIZ,NPE)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K,N,J
      INTEGER  N1,N2,NN
C-----------------------------------------------
      my_real
     .   OFF_L
C-----------------------------------------------
      OFF_L = 0.
      DO I=1,NEL
Cf small3b        IF(OFF(I)<1.)OFFG(I) = OFF(I)
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO N=1,NPE
          DO I=1,NEL
            IF(OFFG(I)<ZERO)THEN
              FX(I,N)=ZERO
              FY(I,N)=ZERO
              FZ(I,N)=ZERO
              STIG(I,N)=ZERO
           ENDIF
          ENDDO
        ENDDO
      ENDIF

      IF(JTHE < 0 ) THEN
       IF(OFF_L<=ZERO)THEN
        DO J=1,NPE
         DO I=1,NEL
          IF(OFFG(I)<=ZERO)THEN
             THEM(I,J)=ZERO
          ENDIF
         ENDDO
        ENDDO
       ENDIF
       IF(NODADT_THERM == 1) THEN
        IF(OFF_L<ZERO)THEN
         DO J=1,NPE
          DO I=1,NEL
           IF(OFFG(I)<ZERO)THEN
             CONDEG(I,J)=ZERO
           ENDIF
          ENDDO
         ENDDO
        ENDIF
       ENDIF  
      ENDIF

      IF(IVECTOR==1) THEN
        DO N= 1,8
#include "vectorize.inc"
         DO I=1,NEL
C          II=I+NFT
          K = IADS(N,I)
          FSKYV(K,1)=FX(I,N)
          FSKYV(K,2)=FY(I,N)
          FSKYV(K,3)=FZ(I,N)
          FSKYV(K,7)=STIG(I,N)
         ENDDO
        ENDDO
        DO N= 1,NPE-8
         N1=IPERM1(N+8)
         N2=IPERM2(N+8)
         DO I=1,NEL
C         II=I+NFT-NUMELS+NUMELS20
          NN = NC(I,N+8)
          IF(NN/=0)THEN
            K = IADSP(N,I)
            FSKYV(K,1)=FX(I,N+8)
            FSKYV(K,2)=FY(I,N+8)
            FSKYV(K,3)=FZ(I,N+8)
            FSKYV(K,7)=STIG(I,N+8)
          ELSE
            K = IADS(N1,I)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+8)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+8)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+8)
            FSKYV(K,7)=FSKYV(K,7)+HALF*STIG(I,N+8)    
            K = IADS(N2,I)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+8)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+8)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+8)
            FSKYV(K,7)=FSKYV(K,7)+HALF*STIG(I,N+8)    
          ENDIF
         ENDDO
        ENDDO
      ELSE
        DO N= 1,8
         DO I=1,NEL
          K = IADS(N,I)
          FSKY(1,K)=FX(I,N)
          FSKY(2,K)=FY(I,N)
          FSKY(3,K)=FZ(I,N)
          FSKY(7,K)=STIG(I,N)
         ENDDO
        ENDDO
        DO N= 1,NPE-8
         N1=IPERM1(N+8)
         N2=IPERM2(N+8)
         DO I=1,NEL
C         II=I+NFT-NUMELS+NUMELS20
          NN = NC(I,N+8)
          IF(NN/=0)THEN
            K = IADSP(N,I)
            FSKY(1,K)=FX(I,N+8)
            FSKY(2,K)=FY(I,N+8)
            FSKY(3,K)=FZ(I,N+8)
            FSKY(7,K)=STIG(I,N+8)
          ELSE
            K = IADS(N1,I)
            FSKY(1,K)=FSKY(1,K)+HALF*FX(I,N+8)
            FSKY(2,K)=FSKY(2,K)+HALF*FY(I,N+8)
            FSKY(3,K)=FSKY(3,K)+HALF*FZ(I,N+8)
            FSKY(7,K)=FSKY(7,K)+HALF*STIG(I,N+8)    
            K = IADS(N2,I)
            FSKY(1,K)=FSKY(1,K)+HALF*FX(I,N+8)
            FSKY(2,K)=FSKY(2,K)+HALF*FY(I,N+8)
            FSKY(3,K)=FSKY(3,K)+HALF*FZ(I,N+8)
            FSKY(7,K)=FSKY(7,K)+HALF*STIG(I,N+8)    
          ENDIF
         ENDDO
        ENDDO
      ENDIF
C
      IF(JTHE < 0 ) THEN
        DO N= 1,8
         DO I=1,NEL
          K = IADS(N,I)
          FTHESKY(K)=THEM(I,N)
         ENDDO
        ENDDO
        DO N= 1,NPE-8
         N1=IPERM1(N+8)
         N2=IPERM2(N+8)
         DO I=1,NEL
C         II=I+NFT-NUMELS+NUMELS20
          NN = NC(I,N+8)
          IF(NN/=0)THEN
            K = IADSP(N,I)
            FTHESKY(K)=THEM(I,N+8)
          ELSE
            K = IADS(N1,I)
            FTHESKY(K)=FTHESKY(K) + HALF*THEM(I,N+8)   
            K = IADS(N2,I)
            FTHESKY(K)=FTHESKY(K) + HALF*THEM(I,N+8)    
          ENDIF
         ENDDO
        ENDDO

        IF(NODADT_THERM == 1) THEN
        DO N= 1,8
         DO I=1,NEL
          K = IADS(N,I)
          CONDNSKY(K)=CONDEG(I,N)
         ENDDO
        ENDDO
        DO N= 1,NPE-8
         N1=IPERM1(N+8)
         N2=IPERM2(N+8)
         DO I=1,NEL
C         II=I+NFT-NUMELS+NUMELS20
          NN = NC(I,N+8)
          IF(NN/=0)THEN
            K = IADSP(N,I)
            CONDNSKY(K)=CONDEG(I,N+8)
          ELSE
            K = IADS(N1,I)
            CONDNSKY(K)=CONDNSKY(K) + HALF*CONDEG(I,N+8)   
            K = IADS(N2,I)
            CONDNSKY(K)=CONDNSKY(K) + HALF*CONDEG(I,N+8)    
          ENDIF
         ENDDO
        ENDDO
        ENDIF

      ENDIF
      IF(NSECT>0)THEN
        DO N= 1,NPE-8
          N1=IPERM1(N+8)
          N2=IPERM2(N+8)
          DO I=1,NEL
           NN = NC(I,N+8)
           IF(NN==0)THEN
             FX(I,N1)=FX(I,N1)+HALF*FX(I,N+8)
             FY(I,N1)=FY(I,N1)+HALF*FY(I,N+8)
             FZ(I,N1)=FZ(I,N1)+HALF*FZ(I,N+8)
             FX(I,N2)=FX(I,N2)+HALF*FX(I,N+8)
             FY(I,N2)=FY(I,N2)+HALF*FY(I,N+8)
             FZ(I,N2)=FZ(I,N2)+HALF*FZ(I,N+8)
           END IF
          END DO
        END DO
      END IF
C
      RETURN
      END
