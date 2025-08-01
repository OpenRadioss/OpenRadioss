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
!||    s10cumu3p   ../engine/source/elements/solid/solide10/s10cumu3p.F
!||--- called by ------------------------------------------------------
!||    s10forc3    ../engine/source/elements/solid/solide10/s10forc3.F
!||====================================================================
      SUBROUTINE S10CUMU3P(
     1   OFFG,    STI,     FSKY,    FSKYV,
     2   IADS,    FX,      FY,      FZ,
     3   DELTAX2, IADS10,  NC,      THEM,
     4   FTHESKY, AR,      X,       SAV,
     5   CONDNSKY,CONDE,   ITAGDN,  NEL,
     6   NFT,     ISMSTR,  JTHE,    ISROT,  NODADT_THERM)
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
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: ISROT
      INTEGER, INTENT(IN) :: NODADT_THERM
      INTEGER ITAGDN(*),NEL
C     REAL
      my_real
     .   OFFG(*),FSKYV(LSKY,8),FSKY(8,LSKY),STI(*),DELTAX2(*),
     .   FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10),THEM(MVSIZ,10),
     .   FTHESKY(*),AR(3,*),X(3,*), CONDNSKY(*),CONDE(*)
      DOUBLE PRECISION
     .  SAV(NEL,30)
      INTEGER IADS(8,*),IADS10(6,*)
      INTEGER NC(MVSIZ,10)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, K,N,J
      INTEGER  IPERM(4),IPERM1(6),IPERM2(6),N1,N2,NN,JJ,L1,L2,K1,K2
      my_real
     .        STIV(MVSIZ),STIE(MVSIZ)
C-----------------------------------------------
      DATA IPERM/1,3,6,5/
      DATA IPERM1/1,2,3,1,2,3/
      DATA IPERM2/2,3,1,4,4,4/
      my_real
     .   OFF_L,XM,YM,ZM,XX,YY,ZZ,FACIROT,FACIROT2
C-----------------------------------------------
c     FACIROT  = (7./48.) / (1./32/)  ! rapport des masses
      FACIROT  = (NINE + THIRD) 
c     FACIROT2 = TWO * (7./48.) / (1./32/)  ! 2 * rapport des masses
c     FACIROT2 = NINE + THIRD
      FACIROT2 = TWO * (NINE + THIRD)

      OFF_L = 0.
      DO I=1,NEL
        OFF_L = MIN(OFF_L,OFFG(I))
      ENDDO
      IF(OFF_L<ZERO)THEN
        DO N=1,10
          DO I=1,NEL
            IF(OFFG(I)<ZERO)THEN
              FX(I,N)=ZERO
              FY(I,N)=ZERO
              FZ(I,N)=ZERO
              STI(I)=ZERO
           ENDIF
          ENDDO
        ENDDO
      ENDIF
      IF(JTHE < 0 ) THEN
       IF(OFF_L<ZERO)THEN
        DO J=1,10
         DO I=1,NEL
          IF(OFFG(I)<ZERO)THEN
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
      IF(IDT1TET10/=0 .AND. ISROT/=1)THEN
      ELSE
C       same as version 44./ to be checked
        DO I=1,NEL
          STI(I)=FOURTH*STI(I)
        END DO
      END IF
C
      IF(IVECTOR==1) THEN
        IF(IDT1TET10/=0 .AND. ISROT/=1)THEN
          IF(ISROT == 0)THEN

            DO I=1,NEL
C
C             DELTAX/SSP = 2/Omega, Omega=SQRT[Spectral Radius(M-1 K)]    cf s10deri3.F
C                = SQRT[Volp*Rho/Kp]          cf mqviscb.F
C             STIG = sum(Kp)              cf s10fint3.F
C                => STIG == sum( Volp*rho ) *  Omega**2/4 == M * Omega**2/4
C
C             cf Assembling respectively Kvertex=Mvertex * Omega**2/2 and Kedge=Medge * Omega**2/2
              STIV(I) = TWO/THIRTY2 * STI(I)
              STIE(I) = TWO*SEVEN/FOURTY8 * STI(I)
            END DO

            DO N= 1,4
#include "vectorize.inc"
              DO I=1,NEL
               II=I+NFT
               K = IADS(IPERM(N),II)
               FSKYV(K,1)=FX(I,N)
               FSKYV(K,2)=FY(I,N)
               FSKYV(K,3)=FZ(I,N)

               FSKYV(K,4)=ZERO
               FSKYV(K,5)=ZERO
               FSKYV(K,6)=ZERO
               FSKYV(K,7)=STIV(I)
               FSKYV(K,8)=ZERO
              ENDDO
            ENDDO

            DO N= 1,6
              L1=IPERM(IPERM1(N))
              L2=IPERM(IPERM2(N))
              DO I=1,NEL
               II=I+NFT
               JJ=II-NUMELS8
               NN = NC(I,N+4)
               IF(NN/=0)THEN
           K = IADS10(N,JJ)
           FSKYV(K,1)=FX(I,N+4)
           FSKYV(K,2)=FY(I,N+4)
           FSKYV(K,3)=FZ(I,N+4)
           FSKYV(K,4)=ZERO
           FSKYV(K,5)=ZERO
           FSKYV(K,6)=ZERO    
           FSKYV(K,7)=STIE(I)
               ELSE
           K = IADS(L1,II)
           FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
           FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
           FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
           FSKYV(K,4)=ZERO
           FSKYV(K,5)=ZERO
           FSKYV(K,6)=ZERO
           FSKYV(K,7)=FSKYV(K,7)+HALF*STIE(I)
           K = IADS(L2,II)
           FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
           FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
           FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
           FSKYV(K,4)=ZERO
           FSKYV(K,5)=ZERO
           FSKYV(K,6)=ZERO
           FSKYV(K,7)=FSKYV(K,7)+HALF*STIE(I)
               ENDIF
              ENDDO
            ENDDO
          ELSEIF(ISROT == 2)THEN
            DO I=1,NEL
C
C             DELTAX/SSP = 2/Omega, Omega=SQRT[Spectral Radius(K)/Mmin] with Mmin=M/4   cf s10deri3.F
C                = SQRT[Volp*Rho/Kp]               cf mqviscb.F
C             STIG = sum(Kp)                   cf s10fint3.F
C                => STIG == sum( Volp*rho ) *  Radius(K) / (4 Mmin) == Radius(K)
              STI(I) = HALF * STI(I)
            END DO

            DO N= 1,4
#include "vectorize.inc"
              DO I=1,NEL
               II=I+NFT
               K = IADS(IPERM(N),II)
               FSKYV(K,1)=FX(I,N)
               FSKYV(K,2)=FY(I,N)
               FSKYV(K,3)=FZ(I,N)

               FSKYV(K,4)=ZERO
               FSKYV(K,5)=ZERO
               FSKYV(K,6)=ZERO
               FSKYV(K,7)=STI(I)
               FSKYV(K,8)=ZERO
              ENDDO
            ENDDO

            DO N= 1,6
              K1=IPERM1(N)
              K2=IPERM2(N)
              L1=IPERM(K1)
              L2=IPERM(K2)
              DO I=1,NEL
          N1=NC(I,K1)
          N2=NC(I,K2)
          II=I+NFT
          JJ=II-NUMELS8
          NN = NC(I,N+4)
               IF(NN == 0)THEN

          K = IADS(L1,II)
          FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
          FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
          FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
          K = IADS(L2,II)
          FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
          FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
          FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)

               ELSEIF(ITAGDN(NN)/=0) THEN
            K = IADS10(N,JJ)
            FSKYV(K,1)=FX(I,N+4)
            FSKYV(K,2)=FY(I,N+4)
            FSKYV(K,3)=FZ(I,N+4)
            FSKYV(K,7)=STI(I)*FACIROT
               ENDIF
              ENDDO
            ENDDO
          ENDIF
        ELSEIF(ISROT == 0)THEN ! IF(IDT1TET10/=0 .AND. ISROT/=1)THEN
         DO N= 1,4
#include "vectorize.inc"
          DO I=1,NEL
           II=I+NFT
           K = IADS(IPERM(N),II)
           FSKYV(K,1)=FX(I,N)
           FSKYV(K,2)=FY(I,N)
           FSKYV(K,3)=FZ(I,N)

           FSKYV(K,4)=ZERO
           FSKYV(K,5)=ZERO
           FSKYV(K,6)=ZERO
           FSKYV(K,7)=STI(I)*DELTAX2(I)
           FSKYV(K,8)=ZERO
          ENDDO
         ENDDO

         DO N= 1,6
          L1=IPERM(IPERM1(N))
          L2=IPERM(IPERM2(N))
          DO I=1,NEL
           II=I+NFT
           JJ=II-NUMELS8
           NN = NC(I,N+4)
           IF(NN/=0)THEN
             K = IADS10(N,JJ)
             FSKYV(K,1)=FX(I,N+4)
             FSKYV(K,2)=FY(I,N+4)
             FSKYV(K,3)=FZ(I,N+4)
             FSKYV(K,4)=ZERO
             FSKYV(K,5)=ZERO
             FSKYV(K,6)=ZERO    
             FSKYV(K,7)=STI(I)
           ELSE
             K = IADS(L1,II)
             FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
             FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
             FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
             FSKYV(K,4)=ZERO
             FSKYV(K,5)=ZERO
             FSKYV(K,6)=ZERO
             FSKYV(K,7)=FSKYV(K,7)+HALF*STI(I)
             K = IADS(L2,II)
             FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
             FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
             FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
             FSKYV(K,4)=ZERO
             FSKYV(K,5)=ZERO
             FSKYV(K,6)=ZERO
             FSKYV(K,7)=FSKYV(K,7)+HALF*STI(I)
           ENDIF
          ENDDO
         ENDDO
        ELSEIF(ISROT == 1)THEN
         DO N= 1,4
#include "vectorize.inc"
          DO I=1,NEL
           II=I+NFT
           K = IADS(IPERM(N),II)
           FSKYV(K,1)=FX(I,N)
           FSKYV(K,2)=FY(I,N)
           FSKYV(K,3)=FZ(I,N)

           FSKYV(K,4)=ZERO
           FSKYV(K,5)=ZERO
           FSKYV(K,6)=ZERO
           FSKYV(K,7)=STI(I)*TWO
           FSKYV(K,8)=STI(I)*DELTAX2(I)*ONE_OVER_8*THREE
          ENDDO
         ENDDO

         IF(ISMSTR==1.OR.((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3))THEN
         DO N= 1,6
          K1=IPERM1(N)
          K2=IPERM2(N)
          L1=IPERM(K1)
          L2=IPERM(K2)
          DO I=1,NEL
            N1=NC(I,K1)
            N2=NC(I,K2)
            II=I+NFT
            JJ=II-NUMELS8
            NN = NC(I,N+4)
              IF(ABS(OFFG(I))>ONE)THEN
               XX=SAV(I,K2)-SAV(I,K1)
               YY=SAV(I,K2+10)-SAV(I,K1+10)
               ZZ=SAV(I,K2+20)-SAV(I,K1+20)
               XM = ONE_OVER_8*(YY*FZ(I,N+4) - ZZ*FY(I,N+4))
               YM = ONE_OVER_8*(ZZ*FX(I,N+4) - XX*FZ(I,N+4))
               ZM = ONE_OVER_8*(XX*FY(I,N+4) - YY*FX(I,N+4))
              ELSE
             XM = ONE_OVER_8*
     .       ((X(2,N2)-X(2,N1))*FZ(I,N+4) - (X(3,N2)-X(3,N1))*FY(I,N+4))
             YM = ONE_OVER_8*
     .       ((X(3,N2)-X(3,N1))*FX(I,N+4) - (X(1,N2)-X(1,N1))*FZ(I,N+4))
             ZM = ONE_OVER_8*
     .       ((X(1,N2)-X(1,N1))*FY(I,N+4) - (X(2,N2)-X(2,N1))*FX(I,N+4))
            END IF            
            K = IADS(L1,II)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
            FSKYV(K,4)=FSKYV(K,4) + XM
            FSKYV(K,5)=FSKYV(K,5) + YM
            FSKYV(K,6)=FSKYV(K,6) + ZM
            K = IADS(L2,II)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
            FSKYV(K,4)=FSKYV(K,4) - XM
            FSKYV(K,5)=FSKYV(K,5) - YM
            FSKYV(K,6)=FSKYV(K,6) - ZM
          ENDDO
         ENDDO
         ELSE
         DO N= 1,6
          K1=IPERM1(N)
          K2=IPERM2(N)
          L1=IPERM(K1)
          L2=IPERM(K2)
          DO I=1,NEL
            N1=NC(I,K1)
            N2=NC(I,K2)
            II=I+NFT
            JJ=II-NUMELS8
            NN = NC(I,N+4)
            XM = ONE_OVER_8*
     .      ((X(2,N2)-X(2,N1))*FZ(I,N+4) - (X(3,N2)-X(3,N1))*FY(I,N+4)) 
            YM = ONE_OVER_8*
     .      ((X(3,N2)-X(3,N1))*FX(I,N+4) - (X(1,N2)-X(1,N1))*FZ(I,N+4)) 
            ZM = ONE_OVER_8*
     .      ((X(1,N2)-X(1,N1))*FY(I,N+4) - (X(2,N2)-X(2,N1))*FX(I,N+4)) 

            K = IADS(L1,II)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
            FSKYV(K,4)=FSKYV(K,4) + XM
            FSKYV(K,5)=FSKYV(K,5) + YM
            FSKYV(K,6)=FSKYV(K,6) + ZM
            K = IADS(L2,II)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
            FSKYV(K,4)=FSKYV(K,4) - XM
            FSKYV(K,5)=FSKYV(K,5) - YM
            FSKYV(K,6)=FSKYV(K,6) - ZM
          ENDDO
         ENDDO
         END IF
        ELSEIF(ISROT == 2)THEN
         DO N= 1,4
#include "vectorize.inc"
          DO I=1,NEL
           II=I+NFT
           K = IADS(IPERM(N),II)
           FSKYV(K,1)=FX(I,N)
           FSKYV(K,2)=FY(I,N)
           FSKYV(K,3)=FZ(I,N)

           FSKYV(K,4)=ZERO
           FSKYV(K,5)=ZERO
           FSKYV(K,6)=ZERO
           FSKYV(K,7)=STI(I)*TWO
           FSKYV(K,8)=ZERO
          ENDDO
         ENDDO

         DO N= 1,6
          K1=IPERM1(N)
          K2=IPERM2(N)
          L1=IPERM(K1)
          L2=IPERM(K2)
          DO I=1,NEL
            N1=NC(I,K1)
            N2=NC(I,K2)
            II=I+NFT
            JJ=II-NUMELS8
            NN = NC(I,N+4)
           IF(NN == 0)THEN

            K = IADS(L1,II)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)
            K = IADS(L2,II)
            FSKYV(K,1)=FSKYV(K,1)+HALF*FX(I,N+4)
            FSKYV(K,2)=FSKYV(K,2)+HALF*FY(I,N+4)
            FSKYV(K,3)=FSKYV(K,3)+HALF*FZ(I,N+4)

           ELSEIF(ITAGDN(NN)/=0) THEN
              K = IADS10(N,JJ)
              FSKYV(K,1)=FX(I,N+4)
              FSKYV(K,2)=FY(I,N+4)
              FSKYV(K,3)=FZ(I,N+4)
              FSKYV(K,7)=STI(I)*FACIROT2
           ENDIF
          ENDDO
         ENDDO
        ENDIF

      ELSE

        IF(IDT1TET10/=0 .AND. ISROT/=1)THEN
          IF(ISROT == 0)THEN

            DO I=1,NEL
C
C             DELTAX/SSP = 2/Omega, Omega=SQRT[Spectral Radius(M-1 K)]    cf s10deri3.F
C                = SQRT[Volp*Rho/Kp]          cf mqviscb.F
C             STIG = sum(Kp)              cf s10fint3.F
C                => STIG == sum( Volp*rho ) *  Omega**2/4 == M * Omega**2/4
C
C             cf Assembling respectively Kvertex=Mvertex * Omega**2/2 and Kedge=Medge * Omega**2/2
              STIV(I) = TWO/THIRTY2 * STI(I)
              STIE(I) = TWO*SEVEN/FOURTY8 * STI(I)
            END DO

            DO N= 1,4
             DO I=1,NEL
              II=I+NFT
              K = IADS(IPERM(N),II)
              FSKY(1,K)=FX(I,N)
              FSKY(2,K)=FY(I,N)
              FSKY(3,K)=FZ(I,N)
              FSKY(4,K)=ZERO
              FSKY(5,K)=ZERO
              FSKY(6,K)=ZERO
              FSKY(7,K)=STIV(I)
              FSKY(8,K)=ZERO
             ENDDO
            ENDDO

            DO N= 1,6
             L1=IPERM(IPERM1(N))
             L2=IPERM(IPERM2(N))
             DO I=1,NEL
              II=I+NFT
              JJ=II-NUMELS8
              NN = NC(I,N+4)
              IF(NN/=0)THEN
          K = IADS10(N,JJ)
          FSKY(1,K)=FX(I,N+4)
          FSKY(2,K)=FY(I,N+4)
          FSKY(3,K)=FZ(I,N+4)
          FSKY(7,K)=STIE(I)
              ELSE
          K = IADS(L1,II)
          FSKY(1,K)=FSKY(1,K)+HALF*FX(I,N+4)
          FSKY(2,K)=FSKY(2,K)+HALF*FY(I,N+4)
          FSKY(3,K)=FSKY(3,K)+HALF*FZ(I,N+4)
          FSKY(7,K)=FSKY(7,K) + HALF*STIE(I)
          K = IADS(L2,II)
          FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
          FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
          FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)
          FSKY(7,K)=FSKY(7,K ) + HALF*STIE(I)
              ENDIF
             ENDDO
            ENDDO

          ELSEIF(ISROT == 2)THEN
            DO I=1,NEL
C
C             DELTAX/SSP = 2/Omega, Omega=SQRT[Spectral Radius(M-1 K)]    cf s10deri3.F
C                = SQRT[Volp*Rho/Kp]          cf mqviscb.F
C             STIG = sum(Kp)              cf s10fint3.F
C                => STIG == sum( Volp*rho ) *  Omega**2/4 == M * Omega**2/4
C
C             cf Assembling K = 1/2 * M/4 * Omega**2 
              STI(I) = HALF * STI(I)
            END DO

            DO N= 1,4
             DO I=1,NEL
              II=I+NFT
              K = IADS(IPERM(N),II)
              FSKY(1,K)=FX(I,N)
              FSKY(2,K)=FY(I,N)
              FSKY(3,K)=FZ(I,N)
              FSKY(7,K)=STI(I)
             ENDDO
            ENDDO

            DO N= 1,6
             K1=IPERM1(N)
             K2=IPERM2(N)
             L1=IPERM(K1)
             L2=IPERM(K2)
             DO I=1,NEL
               N1=NC(I,K1)
               N2=NC(I,K2)
               II=I+NFT
               JJ=II-NUMELS8
               NN = NC(I,N+4)
              IF(NN == 0)THEN
               K = IADS(L1,II)
               FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
               FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
               FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)
               K = IADS(L2,II)
               FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
               FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
               FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)

              ELSEIF(ITAGDN(NN)/=0) THEN
               K = IADS10(N,JJ)
               FSKY(1,K) = FX(I,N+4)
               FSKY(2,K) = FY(I,N+4)
               FSKY(3,K) = FZ(I,N+4)
               FSKY(7,K) = STI(I)*FACIROT
              ENDIF

             ENDDO
            ENDDO
          ENDIF

        ELSEIF(ISROT == 0)THEN ! IF(IDT1TET10/=0 .AND. ISROT/=1)THEN
          DO N= 1,4
           DO I=1,NEL
            II=I+NFT
            K = IADS(IPERM(N),II)
            FSKY(1,K)=FX(I,N)
            FSKY(2,K)=FY(I,N)
            FSKY(3,K)=FZ(I,N)
            FSKY(4,K)=ZERO
            FSKY(5,K)=ZERO
            FSKY(6,K)=ZERO
            FSKY(7,K)=STI(I)*DELTAX2(I)
            FSKY(8,K)=ZERO
           ENDDO
          ENDDO

          DO N= 1,6
           L1=IPERM(IPERM1(N))
           L2=IPERM(IPERM2(N))
           DO I=1,NEL
            II=I+NFT
            JJ=II-NUMELS8
            NN = NC(I,N+4)
            IF(NN/=0)THEN
              K = IADS10(N,JJ)
              FSKY(1,K)=FX(I,N+4)
              FSKY(2,K)=FY(I,N+4)
              FSKY(3,K)=FZ(I,N+4)
              FSKY(7,K)=STI(I)
            ELSE
              K = IADS(L1,II)
              FSKY(1,K)=FSKY(1,K)+HALF*FX(I,N+4)
              FSKY(2,K)=FSKY(2,K)+HALF*FY(I,N+4)
              FSKY(3,K)=FSKY(3,K)+HALF*FZ(I,N+4)
              FSKY(7,K)=FSKY(7,K) + HALF*STI(I)
              K = IADS(L2,II)
              FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
              FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
              FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)
              FSKY(7,K)=FSKY(7,K ) + HALF*STI(I)
            ENDIF
           ENDDO
          ENDDO

        ELSEIF(ISROT == 1)THEN

         DO N= 1,4
          DO I=1,NEL
           II=I+NFT
           K = IADS(IPERM(N),II)
           FSKY(1,K)=FX(I,N)
           FSKY(2,K)=FY(I,N)
           FSKY(3,K)=FZ(I,N)
           FSKY(4,K)=ZERO
           FSKY(5,K)=ZERO
           FSKY(6,K)=ZERO
           FSKY(7,K)=STI(I)*TWO
           FSKY(8,K)=STI(I)*DELTAX2(I)*ONE_OVER_8*THREE
          ENDDO
         ENDDO

         IF(ISMSTR==1.OR.((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3))THEN
          DO N= 1,6
           K1=IPERM1(N)
           K2=IPERM2(N)
           L1=IPERM(K1)
           L2=IPERM(K2)
           DO I=1,NEL
             N1=NC(I,K1)
             N2=NC(I,K2)
             II=I+NFT
             JJ=II-NUMELS8
             NN = NC(I,N+4)
               IF(ABS(OFFG(I))>ONE)THEN
                XX=SAV(I,K2)-SAV(I,K1)
                YY=SAV(I,K2+10)-SAV(I,K1+10)
                ZZ=SAV(I,K2+20)-SAV(I,K1+20)
                XM = ONE_OVER_8*(YY*FZ(I,N+4) - ZZ*FY(I,N+4))
                YM = ONE_OVER_8*(ZZ*FX(I,N+4) - XX*FZ(I,N+4))
                ZM = ONE_OVER_8*(XX*FY(I,N+4) - YY*FX(I,N+4))
               ELSE
              XM = ONE_OVER_8*
     .       ((X(2,N2)-X(2,N1))*FZ(I,N+4) - (X(3,N2)-X(3,N1))*FY(I,N+4)) 
              YM = ONE_OVER_8*
     .       ((X(3,N2)-X(3,N1))*FX(I,N+4) - (X(1,N2)-X(1,N1))*FZ(I,N+4)) 
              ZM = ONE_OVER_8*
     .       ((X(1,N2)-X(1,N1))*FY(I,N+4) - (X(2,N2)-X(2,N1))*FX(I,N+4)) 
             END IF
             K = IADS(L1,II)
             FSKY(1,K)=FSKY(1,K)+HALF*FX(I,N+4)
             FSKY(2,K)=FSKY(2,K)+HALF*FY(I,N+4)
             FSKY(3,K)=FSKY(3,K)+HALF*FZ(I,N+4)
             FSKY(4,K)=FSKY(4,K) + XM
             FSKY(5,K)=FSKY(5,K) + YM
             FSKY(6,K)=FSKY(6,K) + ZM
             K = IADS(L2,II)
             FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
             FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
             FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)
             FSKY(4,K)=FSKY(4,K) - XM
             FSKY(5,K)=FSKY(5,K) - YM
             FSKY(6,K)=FSKY(6,K) - ZM
           ENDDO
          ENDDO
         ELSE
          DO N= 1,6
           K1=IPERM1(N)
           K2=IPERM2(N)
           L1=IPERM(K1)
           L2=IPERM(K2)
           DO I=1,NEL
             N1=NC(I,K1)
             N2=NC(I,K2)
             II=I+NFT
             JJ=II-NUMELS8
             NN = NC(I,N+4)
             XM = ONE_OVER_8*
     .       ((X(2,N2)-X(2,N1))*FZ(I,N+4) - (X(3,N2)-X(3,N1))*FY(I,N+4)) 
             YM = ONE_OVER_8*
     .       ((X(3,N2)-X(3,N1))*FX(I,N+4) - (X(1,N2)-X(1,N1))*FZ(I,N+4)) 
             ZM = ONE_OVER_8*
     .       ((X(1,N2)-X(1,N1))*FY(I,N+4) - (X(2,N2)-X(2,N1))*FX(I,N+4)) 

             K = IADS(L1,II)
             FSKY(1,K)=FSKY(1,K)+HALF*FX(I,N+4)
             FSKY(2,K)=FSKY(2,K)+HALF*FY(I,N+4)
             FSKY(3,K)=FSKY(3,K)+HALF*FZ(I,N+4)
             FSKY(4,K)=FSKY(4,K) + XM
             FSKY(5,K)=FSKY(5,K) + YM
             FSKY(6,K)=FSKY(6,K) + ZM
             K = IADS(L2,II)
             FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
             FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
             FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)
             FSKY(4,K)=FSKY(4,K) - XM
             FSKY(5,K)=FSKY(5,K) - YM
             FSKY(6,K)=FSKY(6,K) - ZM
           ENDDO
          ENDDO
         END IF
        ELSEIF(ISROT == 2)THEN

         DO N= 1,4
          DO I=1,NEL
           II=I+NFT
           K = IADS(IPERM(N),II)
           FSKY(1,K)=FX(I,N)
           FSKY(2,K)=FY(I,N)
           FSKY(3,K)=FZ(I,N)
           FSKY(7,K)=STI(I)*TWO
          ENDDO
         ENDDO

         DO N= 1,6
          K1=IPERM1(N)
          K2=IPERM2(N)
          L1=IPERM(K1)
          L2=IPERM(K2)
          DO I=1,NEL
            N1=NC(I,K1)
            N2=NC(I,K2)
            II=I+NFT
            JJ=II-NUMELS8
            NN = NC(I,N+4)
           IF(NN == 0)THEN
            K = IADS(L1,II)
            FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
            FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
            FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)
            K = IADS(L2,II)
            FSKY(1,K)=FSKY(1,K) + HALF*FX(I,N+4)
            FSKY(2,K)=FSKY(2,K) + HALF*FY(I,N+4)
            FSKY(3,K)=FSKY(3,K) + HALF*FZ(I,N+4)

           ELSEIF(ITAGDN(NN)/=0) THEN
            K = IADS10(N,JJ)
            FSKY(1,K) = FX(I,N+4)
            FSKY(2,K) = FY(I,N+4)
            FSKY(3,K) = FZ(I,N+4)
            FSKY(7,K) = STI(I)*FACIROT2
           ENDIF

          ENDDO
         ENDDO
        ENDIF
      ENDIF
C
C  heat transfert
C
      IF(JTHE < 0 ) THEN
        DO N= 1,4
           DO I=1,NEL
             II=I+NFT
             K = IADS(IPERM(N),II)
             FTHESKY(K)=THEM(I,N)
           ENDDO
        ENDDO
        IF(ISROT == 0)THEN
         DO N= 1,6
           N1=IPERM1(N)
           N2=IPERM2(N)
           DO I=1,NEL
               II=I+NFT
               JJ=II-NUMELS8
               NN = NC(I,N+4)
               IF(NN/=0)THEN
                  K = IADS10(N,JJ)
                  FTHESKY(K)=THEM(I,N+4)
               ELSE
                  K = IADS(IPERM(N1),II)
                  FTHESKY(K)=FTHESKY(K) + HALF*THEM(I,N+4)            
                  K = IADS(IPERM(N2),II)
                  FTHESKY(K)=FTHESKY(K) + HALF*THEM(I,N+4)            
               ENDIF
           ENDDO
         ENDDO 
        ENDIF 
      ENDIF 
C
      IF(NODADT_THERM == 1) THEN

         DO I=1,NEL
            CONDE(I)=FOURTH*CONDE(I)
         END DO

         IF(ISROT == 0)THEN ! IF(IDT1SOL/=0 .AND. ISROT/=1)THEN
           DO N= 1,4
             DO I=1,NEL
                II=I+NFT
                K = IADS(IPERM(N),II)
                CONDNSKY(K)=CONDE(I)*DELTAX2(I)
            ENDDO
           ENDDO

           DO N= 1,6
             L1=IPERM(IPERM1(N))
             L2=IPERM(IPERM2(N))
             DO I=1,NEL
               II=I+NFT
               JJ=II-NUMELS8
               NN = NC(I,N+4)
               IF(NN/=0)THEN
                 K = IADS10(N,JJ)
                CONDNSKY(K)=CONDE(I)
               ELSE
                K = IADS(L1,II)
                CONDNSKY(K)=CONDNSKY(K) + HALF*CONDE(I)
                K = IADS(L2,II)
                CONDNSKY(K)=CONDNSKY(K) + HALF*CONDE(I)
              ENDIF
             ENDDO
           ENDDO
         ELSEIF(ISROT == 1)THEN
           DO N= 1,4
             DO I=1,NEL
               II=I+NFT
                K = IADS(IPERM(N),II)
                CONDNSKY(K)=CONDE(I)*DELTAX2(I)*ONE_OVER_8*THREE
             ENDDO
           ENDDO   
         ELSEIF(ISROT == 2)THEN
          DO N= 1,4
           DO I=1,NEL
            II=I+NFT
            K = IADS(IPERM(N),II)
            CONDNSKY(K)=CONDE(I)*TWO
           ENDDO
          ENDDO

          DO N= 1,6
            K1=IPERM1(N)
            K2=IPERM2(N)
            L1=IPERM(K1)
            L2=IPERM(K2)
            DO I=1,NEL
              N1=NC(I,K1)
              N2=NC(I,K2)
              II=I+NFT
              JJ=II-NUMELS8
              NN = NC(I,N+4)
              IF(NN /= 0.AND.ITAGDN(NN)/=0)THEN
                K = IADS10(N,JJ)
               CONDNSKY(K)=CONDE(I)*FACIROT2

              ENDIF
            ENDDO
          ENDDO
        ENDIF    
      ENDIF
C      
      IF(NSECT>0)THEN
        DO N= 1,6
          N1=IPERM1(N)
          N2=IPERM2(N)
          DO I=1,NEL
           NN = NC(I,N+4)
           IF(NN==0)THEN
             FX(I,N1)=FX(I,N1)+HALF*FX(I,N+4)
             FY(I,N1)=FY(I,N1)+HALF*FY(I,N+4)
             FZ(I,N1)=FZ(I,N1)+HALF*FZ(I,N+4)
             FX(I,N2)=FX(I,N2)+HALF*FX(I,N+4)
             FY(I,N2)=FY(I,N2)+HALF*FY(I,N+4)
             FZ(I,N2)=FZ(I,N2)+HALF*FZ(I,N+4)
           END IF
          END DO
        END DO
      END IF
C
      RETURN
      END
