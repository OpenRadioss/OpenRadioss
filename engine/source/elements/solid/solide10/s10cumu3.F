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
Chd|  S10CUMU3                      source/elements/solid/solide10/s10cumu3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10CUMU3(
     1   OFFG,    A,       NC,      STIFN,
     2   STI,     FX,      FY,      FZ,
     3   DELTAX2, THEM,    FTHE,    AR,
     4   X,       STIFR,   SAV,     CONDN,
     5   CONDE,   ITAGDN,  NEL,     ISMSTR,
     6   JTHE,    ISROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JTHE
      INTEGER, INTENT(IN) :: ISROT
      INTEGER NC(MVSIZ,10),ITAGDN(*),NEL
C     REAL
      my_real
     .   OFFG(*),A(3,*),STIFN(*),STI(*),DELTAX2(*),
     .   FX(MVSIZ,10), FY(MVSIZ,10), FZ(MVSIZ,10),
     .   THEM(MVSIZ,10),FTHE(*),AR(3,*),X(3,*),STIFR(*), 
     .   CONDN(*),CONDE(*)
      DOUBLE PRECISION
     .  SAV(NEL,30)
      my_real
     .  STIV(MVSIZ),STIE(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,N, IPERM(4),IPERM1(10),IPERM2(10),N1,N2,NN,ND,II,J
C-----------------------------------------------
      DATA IPERM/1,3,6,5/
      DATA IPERM1/0,0,0,0,1,2,3,1,2,3/
      DATA IPERM2/0,0,0,0,2,3,1,4,4,4/
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
        DO I=1,NEL
           IF(OFFG(I)<ZERO)THEN
              FX(I,1:10)=ZERO
              STI(I)=ZERO
           ENDIF
        ENDDO
      ENDIF
      IF(JTHE < 0 ) THEN
       IF(OFF_L<=ZERO)THEN
        DO J=1,10
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
      IF(IDT1TET10/=0 .AND. ISROT/=1)THEN
        IF(ISROT==0)THEN
          DO I=1,NEL
C
C           DELTAX/SSP = 2/Omega, Omega=SQRT[Spectral Radius(M-1 K)]    cf s10deri3.F
C                      = SQRT[Volp*Rho/Kp]                              cf mqviscb.F
C           STIG = sum(Kp)                                              cf s10fint3.F
C                => STIG == sum( Volp*rho ) *  Omega**2/4 == M * Omega**2/4
C
C           cf Assembling respectively Kvertex=Mvertex * Omega**2/2 and Kedge=Medge * Omega**2/2
            STIV(I) = TWO/THIRTY2 * STI(I)
            STIE(I) = TWO*SEVEN/FOURTY8 * STI(I)
          END DO
C
          DO N=1,4
           DO I=1,NEL
             NN = NC(I,N)
             A(1,NN)=A(1,NN)+FX(I,N)
             A(2,NN)=A(2,NN)+FY(I,N)
             A(3,NN)=A(3,NN)+FZ(I,N)
C            Assembling Mvertex * Rayon(M-1 K)/2 
             STIFN(NN)=STIFN(NN)+STIV(I)
           ENDDO
          ENDDO

          DO N=5,10
           DO I=1,NEL
             NN = NC(I,N)
             IF(NN/=0)THEN
               A(1,NN)=A(1,NN)+FX(I,N)
               A(2,NN)=A(2,NN)+FY(I,N)
               A(3,NN)=A(3,NN)+FZ(I,N)
C              Assembling Medge * Rayon(M-1 K)/2
               STIFN(NN)=STIFN(NN)+STIE(I)
             ELSE
               N1=NC(I,IPERM1(N))
               N2=NC(I,IPERM2(N))
               A(1,N1)=A(1,N1)+HALF*FX(I,N)
               A(2,N1)=A(2,N1)+HALF*FY(I,N)
               A(3,N1)=A(3,N1)+HALF*FZ(I,N)
               STIFN(N1)=STIFN(N1)+HALF*STIE(I)
               A(1,N2)=A(1,N2)+HALF*FX(I,N)
               A(2,N2)=A(2,N2)+HALF*FY(I,N)
               A(3,N2)=A(3,N2)+HALF*FZ(I,N)
               STIFN(N2)=STIFN(N2)+HALF*STIE(I)
             ENDIF
           ENDDO
          ENDDO

        ELSE 
          DO I=1,NEL
C
C           DELTAX/SSP = 2/Omega, Omega=SQRT[Spectral Radius(K)/Mmin] with Mmin=M/4    cf s10deri3.F
C                      = SQRT[Volp*Rho/Kp]                                   cf mqviscb.F
C           STIG = sum(Kp)                                                   cf s10fint3.F
C                => STIG == sum( Volp*rho ) *  Radius(K) / (4 Mmin) == Radius(K)
            STI(I) = HALF * STI(I)
          END DO

          DO N=1,4
           DO I=1,NEL
            NN = NC(I,N)
            A(1,NN)=A(1,NN)+FX(I,N)
            A(2,NN)=A(2,NN)+FY(I,N)
            A(3,NN)=A(3,NN)+FZ(I,N)
            STIFN(NN)=STIFN(NN)+STI(I)
           ENDDO
          ENDDO

          DO N=5,10
            DO I=1,NEL
              NN = NC(I,N)
             IF(NN == 0)THEN
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              A(1,N1)=A(1,N1)+HALF*FX(I,N)
              A(2,N1)=A(2,N1)+HALF*FY(I,N)
              A(3,N1)=A(3,N1)+HALF*FZ(I,N)
              A(1,N2)=A(1,N2)+HALF*FX(I,N)
              A(2,N2)=A(2,N2)+HALF*FY(I,N)
              A(3,N2)=A(3,N2)+HALF*FZ(I,N)
             ELSEIF(ITAGDN(NN)/=0) THEN
C-----------will be done in resol    
              A(1,NN)=A(1,NN)+FX(I,N)
              A(2,NN)=A(2,NN)+FY(I,N)
              A(3,NN)=A(3,NN)+FZ(I,N)
              STIFN(NN)=STIFN(NN)+STI(I)*FACIROT
             ENDIF
            ENDDO
          ENDDO
        ENDIF

      ELSE ! IF(IDT1TET10/=0 .AND. ISROT/=1)THEN
C       same as version 44./ to be checked
        DO I=1,NEL
          STI(I)=FOURTH*STI(I)
        END DO
C
        IF(ISROT == 0)THEN
         DO N=1,4
          DO I=1,NEL
            NN = NC(I,N)
            A(1,NN)=A(1,NN)+FX(I,N)
            A(2,NN)=A(2,NN)+FY(I,N)
            A(3,NN)=A(3,NN)+FZ(I,N)
            STIFN(NN)=STIFN(NN)+STI(I)*DELTAX2(I)
          ENDDO
         ENDDO

         DO N=5,10
          DO I=1,NEL
            NN = NC(I,N)
            IF(NN/=0)THEN
              A(1,NN)=A(1,NN)+FX(I,N)
              A(2,NN)=A(2,NN)+FY(I,N)
              A(3,NN)=A(3,NN)+FZ(I,N)
              STIFN(NN)=STIFN(NN)+STI(I)
            ELSE
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              A(1,N1)=A(1,N1)+HALF*FX(I,N)
              A(2,N1)=A(2,N1)+HALF*FY(I,N)
              A(3,N1)=A(3,N1)+HALF*FZ(I,N)
              STIFN(N1)=STIFN(N1)+HALF*STI(I)
              A(1,N2)=A(1,N2)+HALF*FX(I,N)
              A(2,N2)=A(2,N2)+HALF*FY(I,N)
              A(3,N2)=A(3,N2)+HALF*FZ(I,N)
              STIFN(N2)=STIFN(N2)+HALF*STI(I)
            ENDIF
          ENDDO
         ENDDO

        ELSEIF(ISROT == 1)THEN

          DO N=1,4
           DO I=1,NEL
            NN = NC(I,N)
            A(1,NN)=A(1,NN)+FX(I,N)
            A(2,NN)=A(2,NN)+FY(I,N)
            A(3,NN)=A(3,NN)+FZ(I,N)
            STIFN(NN)=STIFN(NN) + STI(I)*TWO
            STIFR(NN)=STIFR(NN) + ONE_OVER_8*STI(I)*DELTAX2(I)*THREE
           ENDDO
          ENDDO
          
          IF(ISMSTR==1.OR.((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3))THEN
           DO N=5,10
            DO I=1,NEL
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              A(1,N1)=A(1,N1)+HALF*FX(I,N)
              A(2,N1)=A(2,N1)+HALF*FY(I,N)
              A(3,N1)=A(3,N1)+HALF*FZ(I,N)
              A(1,N2)=A(1,N2)+HALF*FX(I,N)
              A(2,N2)=A(2,N2)+HALF*FY(I,N)
              A(3,N2)=A(3,N2)+HALF*FZ(I,N)
                IF(ABS(OFFG(I))>ONE)THEN
              XX=SAV(I,IPERM2(N))-SAV(I,IPERM1(N))
              YY=SAV(I,IPERM2(N)+10)-SAV(I,IPERM1(N)+10)
              ZZ=SAV(I,IPERM2(N)+20)-SAV(I,IPERM1(N)+20)
              XM = ONE_OVER_8*(YY*FZ(I,N) - ZZ*FY(I,N))
              YM = ONE_OVER_8*(ZZ*FX(I,N) - XX*FZ(I,N))
              ZM = ONE_OVER_8*(XX*FY(I,N) - YY*FX(I,N))
                ELSE
          XM = ONE_OVER_8*
     .    ((X(2,N2)-X(2,N1))*FZ(I,N) - (X(3,N2)-X(3,N1))*FY(I,N))
          YM = ONE_OVER_8*
     .    ((X(3,N2)-X(3,N1))*FX(I,N) - (X(1,N2)-X(1,N1))*FZ(I,N))
          ZM = ONE_OVER_8*
     .    ((X(1,N2)-X(1,N1))*FY(I,N) - (X(2,N2)-X(2,N1))*FX(I,N))
              END IF
              AR(1,N1) = AR(1,N1) + XM
              AR(2,N1) = AR(2,N1) + YM
              AR(3,N1) = AR(3,N1) + ZM
              AR(1,N2) = AR(1,N2) - XM
              AR(2,N2) = AR(2,N2) - YM
              AR(3,N2) = AR(3,N2) - ZM
            END DO
           END DO
          ELSE
           DO N=5,10
            DO I=1,NEL
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              A(1,N1)=A(1,N1)+HALF*FX(I,N)
              A(2,N1)=A(2,N1)+HALF*FY(I,N)
              A(3,N1)=A(3,N1)+HALF*FZ(I,N)
              A(1,N2)=A(1,N2)+HALF*FX(I,N)
              A(2,N2)=A(2,N2)+HALF*FY(I,N)
              A(3,N2)=A(3,N2)+HALF*FZ(I,N)
              XM = ONE_OVER_8*
     .        ((X(2,N2)-X(2,N1))*FZ(I,N) - (X(3,N2)-X(3,N1))*FY(I,N))
              YM = ONE_OVER_8*
     .        ((X(3,N2)-X(3,N1))*FX(I,N) - (X(1,N2)-X(1,N1))*FZ(I,N))
              ZM = ONE_OVER_8*
     .        ((X(1,N2)-X(1,N1))*FY(I,N) - (X(2,N2)-X(2,N1))*FX(I,N))
              AR(1,N1) = AR(1,N1) + XM
              AR(2,N1) = AR(2,N1) + YM
              AR(3,N1) = AR(3,N1) + ZM
              AR(1,N2) = AR(1,N2) - XM
              AR(2,N2) = AR(2,N2) - YM
              AR(3,N2) = AR(3,N2) - ZM
            ENDDO
           ENDDO
          END IF
        ELSEIF(ISROT == 2)THEN

          DO N=1,4
           DO I=1,NEL
            NN = NC(I,N)
            A(1,NN)=A(1,NN)+FX(I,N)
            A(2,NN)=A(2,NN)+FY(I,N)
            A(3,NN)=A(3,NN)+FZ(I,N)
            STIFN(NN)=STIFN(NN)+STI(I)*TWO
           ENDDO
          ENDDO
          DO N=5,10
            DO I=1,NEL
              NN = NC(I,N)
             IF(NN == 0)THEN
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              A(1,N1)=A(1,N1)+HALF*FX(I,N)
              A(2,N1)=A(2,N1)+HALF*FY(I,N)
              A(3,N1)=A(3,N1)+HALF*FZ(I,N)
              A(1,N2)=A(1,N2)+HALF*FX(I,N)
              A(2,N2)=A(2,N2)+HALF*FY(I,N)
              A(3,N2)=A(3,N2)+HALF*FZ(I,N)
             ELSEIF(ITAGDN(NN)/=0) THEN
C-----------will be done in resol    
              A(1,NN)=A(1,NN)+FX(I,N)
              A(2,NN)=A(2,NN)+FY(I,N)
              A(3,NN)=A(3,NN)+FZ(I,N)
              STIFN(NN)=STIFN(NN)+STI(I)*FACIROT2
             ENDIF
            ENDDO
          ENDDO
        ENDIF
      END IF
C

      IF(JTHE < 0 ) THEN
C
C  + heat transfort
C
        DO N=1,4
          DO I=1,NEL
            NN = NC(I,N)
            FTHE(NN)= FTHE(NN) + THEM(I,N)
          ENDDO
        ENDDO
C
        IF(ISROT == 0)THEN
         DO N=5,10
          DO I=1,NEL
            NN = NC(I,N)
            IF(NN/=0)THEN
              FTHE(NN)= FTHE(NN) + THEM(I,N)
            ELSE
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              FTHE(N1)= FTHE(N1) + HALF*THEM(I,N)
              FTHE(N2)= FTHE(N2) + HALF*THEM(I,N)   
            ENDIF
          ENDDO
         ENDDO
        ENDIF

      ENDIF
C
C  + Thermal time step
C
       IF(NODADT_THERM == 1 ) THEN  

        DO I=1,NEL
          CONDE(I)=FOURTH*CONDE(I)
        END DO


        IF(ISROT == 0)THEN
         DO N=1,4
          DO I=1,NEL
            NN = NC(I,N)
            CONDN(NN)= CONDN(NN) + CONDE(I)*DELTAX2(I)
          ENDDO
         ENDDO

         DO N=5,10
          DO I=1,NEL
            NN = NC(I,N)
            IF(NN/=0)THEN
             CONDN(NN)= CONDN(NN) + CONDE(I)
            ELSE
              N1=NC(I,IPERM1(N))
              N2=NC(I,IPERM2(N))
              CONDN(N1)= CONDN(N1)+HALF*CONDE(I)
              CONDN(N2)= CONDN(N2)+HALF*CONDE(I)
            ENDIF
          ENDDO
         ENDDO

        ELSEIF(ISROT == 1)THEN

          DO N=1,4
           DO I=1,NEL
            NN = NC(I,N)
            CONDN(NN)= CONDN(NN) + CONDE(I)*DELTAX2(I)*THREE*ONE_OVER_8
           ENDDO
          ENDDO
        ELSEIF(ISROT == 2)THEN

          DO N=1,4
           DO I=1,NEL
            NN = NC(I,N)
            CONDN(NN)= CONDN(NN) + CONDE(I)*TWO
           ENDDO
          ENDDO
          DO N=5,10
            DO I=1,NEL
              NN = NC(I,N)
              IF(NN/=0.AND.ITAGDN(NN)/=0) THEN
              CONDN(NN)= CONDN(NN) + CONDE(I)*FACIROT2
             ENDIF
            ENDDO
          ENDDO
        ENDIF


      ENDIF
 
      IF(NSECT>0)THEN
       DO N=5,10
        DO I=1,NEL
          NN = NC(I,N)
          IF(NN==0)THEN
            N1=IPERM1(N)
            N2=IPERM2(N)
            FX(I,N1)=FX(I,N1)+HALF*FX(I,N)
            FY(I,N1)=FY(I,N1)+HALF*FY(I,N)
            FZ(I,N1)=FZ(I,N1)+HALF*FZ(I,N)
            FX(I,N2)=FX(I,N2)+HALF*FX(I,N)
            FY(I,N2)=FY(I,N2)+HALF*FY(I,N)
            FZ(I,N2)=FZ(I,N2)+HALF*FZ(I,N)
          END IF
        END DO
       END DO
      END IF

      RETURN
      END
