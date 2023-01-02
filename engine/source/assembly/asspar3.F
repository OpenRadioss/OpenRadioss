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
Chd|  ASSPAR3                       source/assembly/asspar3.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|====================================================================
      SUBROUTINE ASSPAR3( 
     2   A          ,AR         ,ITASK                 ,NODFT      ,
     3   NODLT      ,STIFN      ,STIFR      ,ITAB      ,FSKY       ,
     4   FSKYV      ,ISKY       ,INDSKY     ,FSKYI     ,
     5   ADSKYI     ,PARTFT     ,PARTLT     ,PARTSAV   ,MS         ,
     6   FTHE       ,FTHESKY    ,FTHESKYI   ,GREFT     ,GRELT      ,
     7   GRESAV     )
C----6---------------------------------------------------------------7---------8
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "parit_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "scr07_c.inc"
#include      "couple_c.inc"
#include      "task_c.inc"
      integer maxbloc
      parameter (maxbloc=1000)
      common/tmparit/nbloc,adbloc(0:maxbloc),nbcol(0:maxbloc),
     .       nbdone(maxbloc),ideb(PARASIZ),ifin(PARASIZ)
      integer nbloc,adbloc,nbcol,nbdone,ideb,ifin
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real 
     .   A(3,*)   ,AR(3,*), STIFN(*), STIFR(*),FSKYV(LSKY,8),
     .   FSKYI(LSKYI,4),PARTSAV(*),FSKY(8,LSKY), MS(*),
     .   FTHE(*), FTHESKY(*),FTHESKYI(LSKYI),GRESAV(*)
      INTEGER ITASK,NODFT,NODLT,PARTFT,PARTLT,GREFT,GRELT
      INTEGER ISKY(*),INDSKY(0:*),ADSKYI(0:*),ITAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,KK,J1,J2,JJ1,JJ2,NN,KM,NUM7,LL,
     .        NISKYFT,NISKYLT, LDONE,IDONE,NC,NL,IFT,ILT,KFT,KLT,
     .        K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,I0,IBAR,KKK,NCT
      my_real 
     .        FF, FSKYT(NISKY), FTHESKYT(NISKY)  
      COMMON /ASSP2/ LDONE, IDONE, IBAR
C-----------------------------------------------
      IF(NTHREAD*NISKY+NUMNOD+2>LENWA)THEN
        WRITE(IOUT,*) ' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        WRITE(ISTDO,*)' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        TSTOP=ZERO
        RETURN
      ENDIF
      IF(NISKY>LSKYI)THEN
        WRITE(IOUT,*) ' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        WRITE(ISTDO,*)' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        TSTOP=ZERO
        RETURN
      ENDIF
      NISKYFT   = 1+ITASK*NISKY/ NTHREAD
      NISKYLT   = (ITASK+1)*NISKY/NTHREAD
c      print *,' nisky=',nisky,' nodlt=',nodlt
C
      DO N=NODFT,NODLT
        ADSKYI(N) = 0
      ENDDO
      ADSKYI(NUMNOD+1) = 0
C
      IDONE = 0
      LDONE = 0
      IBAR  = 0
      DO  L=1,NBLOC
       NBDONE(L) = -1
      ENDDO
      CALL MY_BARRIER
C-----------------------------------------------
C     FORCES D'INTERFACES
C-----------------------------------------------
#include "lockon.inc"
      IF(IDONE/=0)THEN
#include "lockoff.inc"
      ELSE
        IDONE = 1
#include "lockoff.inc"
C
        DO I=1,NISKY
          N = ISKY(I) +1 
          ADSKYI(N) = ADSKYI(N) + 1
        ENDDO
C-----------------------------------------------
C   CALCUL DES ADRESSES DU VECTEUR SKYLINE
C-----------------------------------------------
        ADSKYI(0) = 1
        ADSKYI(1) = 1
        DO N=1,NUMNOD
          NN = N+1
          ADSKYI(NN) = ADSKYI(NN) + ADSKYI(N)
        ENDDO
C-----------------------------------------------
C   TRI DES FORCES EN SKYLINE
C-----------------------------------------------
        DO I=1,NISKY
          N = ISKY(I)
          J  = ADSKYI(N)
          ISKY(I)  = J
          ADSKYI(N) = ADSKYI(N) + 1
        ENDDO
C
#include "lockon.inc"
          IDONE = 2
#include "lockoff.inc"
      ENDIF
C-----------------------------------------------
C     FORCES D'ELEMENTS 
C-----------------------------------------------

      IF(IVECTOR==1) THEN
      I0=0
      DO  L=1,NBLOC
       NC  = NBCOL(L) 
       NL  = (ADBLOC(L+1)-ADBLOC(L))/NC                                         
       DO 300 IFT = 0,NL-1,NVSIZ
#include "lockon.inc"
         IF(NBDONE(L)>=IFT)THEN
#include "lockoff.inc"
           GOTO 300
         ENDIF
         NBDONE(L) = IFT
#include "lockoff.inc"
       ILT = MIN(IFT+NVSIZ, NL) - 1
       I   = I0 + IFT
       KFT = ADBLOC(L) + IFT*NC
       KLT = ADBLOC(L) + ILT*NC
       DO  KKK=KFT,KLT,NC
        I=I+1
        N=INDSKY(I)
C
        IF(ITHERM_FE == 0 ) THEN
         DO K=KKK,KKK+NC-2
            DO KK=KKK+1,KKK+NC-1
              DO LL=1,8
                IF(FSKYV(KK,LL)<FSKYV(K,LL))THEN
                  FF = FSKYV(KK,LL)
                  FSKYV(KK,LL) = FSKYV(K,LL)
                  FSKYV(K,LL)  = FF
                ENDIF
              ENDDO         
            ENDDO
          ENDDO
         ELSE
          DO K=KKK,KKK+NC-2
            DO KK=KKK+1,KKK+NC-1
              DO LL=1,8
                IF(FSKYV(KK,LL)<FSKYV(K,LL))THEN
                  FF = FSKYV(KK,LL)
                  FSKYV(KK,LL) = FSKYV(K,LL)
                  FSKYV(K,LL)  = FF
                ENDIF
              ENDDO 
              IF(FTHESKY(KK)<FTHESKY(K))THEN
                  FF = FTHESKY(KK)
                  FTHESKY(KK) = FTHESKY(K)
                  FTHESKY(K)  = FF
              ENDIF            
            ENDDO
          ENDDO
         ENDIF 
                 
C
         IF(ITHERM_FE == 0 ) THEN
          DO K=KKK,KKK+NC-1
           A(1,N)  = A(1,N)  + MAX(ZERO,FSKYV(K,1))
           A(2,N)  = A(2,N)  + MAX(ZERO,FSKYV(K,2))
           A(3,N)  = A(3,N)  + MAX(ZERO,FSKYV(K,3))
           AR(1,N)  = AR(1,N)  + MAX(ZERO,FSKYV(K,4))
           AR(2,N)  = AR(2,N)  + MAX(ZERO,FSKYV(K,5))
           AR(3,N)  = AR(3,N)  + MAX(ZERO,FSKYV(K,6))
           STIFN(N) = STIFN(N) + MAX(ZERO,FSKYV(K,7))
           STIFR(N) = STIFR(N) + MAX(ZERO,FSKYV(K,8))
          ENDDO
          DO K=KKK+NC-1,KKK,-1
            A(1,N)  = A(1,N)  + MIN(ZERO,FSKYV(K,1))
            A(2,N)  = A(2,N)  + MIN(ZERO,FSKYV(K,2))
            A(3,N)  = A(3,N)  + MIN(ZERO,FSKYV(K,3))
            AR(1,N)  = AR(1,N)  + MIN(ZERO,FSKYV(K,4))
            AR(2,N)  = AR(2,N)  + MIN(ZERO,FSKYV(K,5))
            AR(3,N)  = AR(3,N)  + MIN(ZERO,FSKYV(K,6))
          ENDDO 
         ELSE
          DO K=KKK,KKK+NC-1
           A(1,N)  = A(1,N)  + MAX(ZERO,FSKYV(K,1))
           A(2,N)  = A(2,N)  + MAX(ZERO,FSKYV(K,2))
           A(3,N)  = A(3,N)  + MAX(ZERO,FSKYV(K,3))
           AR(1,N)  = AR(1,N)  + MAX(ZERO,FSKYV(K,4))
           AR(2,N)  = AR(2,N)  + MAX(ZERO,FSKYV(K,5))
           AR(3,N)  = AR(3,N)  + MAX(ZERO,FSKYV(K,6))
           STIFN(N) = STIFN(N) + MAX(ZERO,FSKYV(K,7))
           STIFR(N) = STIFR(N) + MAX(ZERO,FSKYV(K,8))
           FTHE(N)   = FTHE(N) + MAX(ZERO,FTHESKY(K))
          ENDDO      
          DO K=KKK+NC-1,KKK,-1
           A(1,N)  = A(1,N)  + MIN(ZERO,FSKYV(K,1))
           A(2,N)  = A(2,N)  + MIN(ZERO,FSKYV(K,2))
           A(3,N)  = A(3,N)  + MIN(ZERO,FSKYV(K,3))
           AR(1,N)  = AR(1,N)  + MIN(ZERO,FSKYV(K,4))
           AR(2,N)  = AR(2,N)  + MIN(ZERO,FSKYV(K,5))
           AR(3,N)  = AR(3,N)  + MIN(ZERO,FSKYV(K,6))
           FTHE(N)   = FTHE(N) + MIN(ZERO,FTHESKY(K))
          ENDDO
         ENDIF          
C
       ENDDO
 300   CONTINUE
       I0 = I0 + NL
      ENDDO
      ELSE
       DO N = IDEB(ITASK+1), IFIN(ITASK+1)
         NCT = INDSKY(N)-1
         NC = INDSKY(N+1)-INDSKY(N)
         IF(ITHERM_FE == 0 )THEN
            DO K = NCT+1, NCT+NC-1
              DO KK=NCT+2,NCT+NC 
                 DO LL=1,8
                   IF(FSKY(LL,KK)<FSKY(LL,K))THEN
                     FF = FSKY(LL,KK)
                     FSKY(LL,KK) = FSKY(LL,K)
                     FSKY(LL,K)  = FF
                   ENDIF
                 ENDDO
               ENDDO
            ENDDO
          ELSE
            DO K = NCT+1, NCT+NC-1
              DO KK=NCT+2,NCT+NC 
                 DO LL=1,8
                   IF(FSKY(LL,KK)<FSKY(LL,K))THEN
                     FF = FSKY(LL,KK)
                     FSKY(LL,KK) = FSKY(LL,K)
                     FSKY(LL,K)  = FF
                   ENDIF
                 ENDDO
                 IF(FTHESKY(KK)<FTHESKY(K))THEN
                     FF = FTHESKY(KK)
                     FTHESKY(KK) = FTHESKY(K)
                     FTHESKY(K)  = FF
                   ENDIF 
               ENDDO
            ENDDO 
          ENDIF  
C
         IF(ITHERM_FE == 0 ) THEN
           DO K=NCT+1, NCT+NC
            A(1,N)  = A(1,N)  + MAX(ZERO,FSKY(1,K))
            A(2,N)  = A(2,N)  + MAX(ZERO,FSKY(2,K))
            A(3,N)  = A(3,N)  + MAX(ZERO,FSKY(3,K))
            AR(1,N)  = AR(1,N)  + MAX(ZERO,FSKY(4,K))
            AR(2,N)  = AR(2,N)  + MAX(ZERO,FSKY(5,K))
            AR(3,N)  = AR(3,N)  + MAX(ZERO,FSKY(6,K))
            STIFN(N) = STIFN(N) + MAX(ZERO,FSKY(7,K))
            STIFR(N) = STIFR(N) + MAX(ZERO,FSKY(8,K))
           ENDDO
           DO K=NCT+NC, NCT+1,-1
            A(1,N)  = A(1,N)  + MIN(ZERO,FSKY(1,K))
            A(2,N)  = A(2,N)  + MIN(ZERO,FSKY(2,K))
            A(3,N)  = A(3,N)  + MIN(ZERO,FSKY(3,K))
            AR(1,N)  = AR(1,N)  + MIN(ZERO,FSKY(4,K))
            AR(2,N)  = AR(2,N)  + MIN(ZERO,FSKY(5,K))
            AR(3,N)  = AR(3,N)  + MIN(ZERO,FSKY(6,K))
           ENDDO
          ELSE
           DO K=NCT+1, NCT+NC
            A(1,N)  = A(1,N)  + MAX(ZERO,FSKY(1,K))
            A(2,N)  = A(2,N)  + MAX(ZERO,FSKY(2,K))
            A(3,N)  = A(3,N)  + MAX(ZERO,FSKY(3,K))
            AR(1,N)  = AR(1,N)  + MAX(ZERO,FSKY(4,K))
            AR(2,N)  = AR(2,N)  + MAX(ZERO,FSKY(5,K))
            AR(3,N)  = AR(3,N)  + MAX(ZERO,FSKY(6,K))
            STIFN(N) = STIFN(N) + MAX(ZERO,FSKY(7,K))
            STIFR(N) = STIFR(N) + MAX(ZERO,FSKY(8,K)) 
            FTHE(N)   = FTHE(N) + MAX(ZERO,FTHESKY(K)) 
           ENDDO
           DO K=NCT+NC, NCT+1,-1
            A(1,N)  = A(1,N)  + MIN(ZERO,FSKY(1,K))
            A(2,N)  = A(2,N)  + MIN(ZERO,FSKY(2,K))
            A(3,N)  = A(3,N)  + MIN(ZERO,FSKY(3,K))
            AR(1,N)  = AR(1,N)  + MIN(ZERO,FSKY(4,K))
            AR(2,N)  = AR(2,N)  + MIN(ZERO,FSKY(5,K))
            AR(3,N)  = AR(3,N)  + MIN(ZERO,FSKY(6,K))
            FTHE(N)   = FTHE(N) + MIN(ZERO,FTHESKY(K)) 
           ENDDO            
          ENDIF       
C
       ENDDO
      ENDIF
C
      IF(N2D/=0) THEN
        CALL MY_BARRIER
           DO I = NODFT, NODLT
             MS(I) = A(1,I)
             A(1,I) = ZERO
           ENDDO
        CALL MY_BARRIER
      ENDIF
C-----------------------------------------------
C     LE CALL BARRIER EST FAIT SUR TOUS LES PROCES. 
C     SI ET SEULEMENT SI LA TACHE IDONE N'EST PAS FAITE 
C     QUAND LE PREMIER DES PROCES. ARRIVE ICI
C-----------------------------------------------
#include "lockon.inc"
        IF(IDONE/=2)IBAR = 1
#include "lockoff.inc"
      IF(IBAR==1)CALL MY_BARRIER
C-----------------------------------------------
      DO L=1,4
#include "lockon.inc"
          IF(LDONE>=L)THEN
#include "lockoff.inc"
          ELSE
            LDONE = L
#include "lockoff.inc"
            DO I=1,NISKY
              J = ISKY(I)
              FSKYT(J) = FSKYI(I,L)
            ENDDO
            DO I=1,NISKY
              FSKYI(I,L) = FSKYT(I)
            ENDDO
C
            IF(INTHEAT > 0 .AND. L == 1) THEN
              DO  I=1,NISKY
               J = ISKY(I)
               FTHESKYT(J) = FTHESKYI(I)      
              ENDDO
              DO  I=1,NISKY
                FTHESKYI(I) = FTHESKYT(I)
              ENDDO
            ENDIF               
C         
          ENDIF
      ENDDO
C
      CALL MY_BARRIER
C
      DO I=NISKYFT,NISKYLT
        ISKY(I) = 0
      ENDDO
      NISKY = 0
C adsKy est decale de 1 
C-----------------------------------------------
C     FORCES D'INTERFACES
C-----------------------------------------------
      DO 800 N=NODFT,NODLT
        NN = N-1
        JJ1 = ADSKYI(NN)
        JJ2 = ADSKYI(N)-1
C-----------------------------------------------
C       TRI DES FORCES D'INTERFACES
C-----------------------------------------------
        IF(INTHEAT == 0) THEN
         DO 500 K=JJ1,JJ2-1
           DO 500 KK=K+1,JJ2
             DO 500 LL=1,4
              IF(FSKYI(KK,LL)<FSKYI(K,LL))THEN
                FF = FSKYI(KK,LL)
                FSKYI(KK,LL) = FSKYI(K,LL)
                FSKYI(K,LL)  = FF
              ENDIF
 500        CONTINUE
C +  la thermique
         ELSE
          DO  K=JJ1,JJ2-1
           DO  KK=K+1,JJ2
             DO  LL=1,4
              IF(FSKYI(KK,LL)<FSKYI(K,LL))THEN
                FF = FSKYI(KK,LL)
                FSKYI(KK,LL) = FSKYI(K,LL)
                FSKYI(K,LL)  = FF
              ENDIF
             ENDDO 
             IF(FTHESKYI(KK) < FTHESKYI(K))THEN
                FF = FTHESKYI(KK)
                FTHESKYI(KK) = FTHESKYI(K)
                FTHESKYI(K)  = FF
             ENDIF
            ENDDO
           ENDDO       
         ENDIF
C-----------------------------------------------
C       ASSEMBLAGE DES FORCES 
C-----------------------------------------------
        IF(INTHEAT == 0 ) THEN
          DO K=JJ1,JJ2
            A(1,N)  = A(1,N)  + MAX(ZERO,FSKYI(K,1))
            A(2,N)  = A(2,N)  + MAX(ZERO,FSKYI(K,2))
            A(3,N)  = A(3,N)  + MAX(ZERO,FSKYI(K,3))
            STIFN(N) = STIFN(N) + FSKYI(K,4)
          ENDDO
          DO K=JJ2,JJ1,-1
            A(1,N)  = A(1,N)  + MIN(ZERO,FSKYI(K,1))
            A(2,N)  = A(2,N)  + MIN(ZERO,FSKYI(K,2))
            A(3,N)  = A(3,N)  + MIN(ZERO,FSKYI(K,3))
          ENDDO
C + la thermique
        ELSE
          DO K=JJ1,JJ2
            A(1,N)  = A(1,N)  + MAX(ZERO,FSKYI(K,1))
            A(2,N)  = A(2,N)  + MAX(ZERO,FSKYI(K,2))
            A(3,N)  = A(3,N)  + MAX(ZERO,FSKYI(K,3))
            STIFN(N) = STIFN(N) + FSKYI(K,4)
            FTHE(N)  = FTHE(N)  + MAX(ZERO,FTHESKYI(K))
          ENDDO
          DO K=JJ2,JJ1,-1
            A(1,N)  = A(1,N)  + MIN(ZERO,FSKYI(K,1))
            A(2,N)  = A(2,N)  + MIN(ZERO,FSKYI(K,2))
            A(3,N)  = A(3,N)  + MIN(ZERO,FSKYI(K,3))
            FTHE(N)  = FTHE(N)  + MIN(ZERO,FTHESKYI(K))
          ENDDO          
        ENDIF      
 800  CONTINUE
C-----------------------------------------------
C
C  003      NUM7 = 7*NPART
      NUM7 = NPSAV*NPART
C
      KM = 0
      DO 950 K=1,NTHREAD-1
          KM = KM + NUM7
          DO 940 I=PARTFT,PARTLT
           PARTSAV(I) = PARTSAV(I) + PARTSAV(I+KM)
           PARTSAV(I+KM) = ZERO
 940      CONTINUE
 950  CONTINUE
C
      CALL MY_BARRIER
C
      NUM7 = NPSAV*NGPE
C
      KM = 0
      IF (NTHPART > 0) THEN
        DO 970 K=1,NTHREAD-1
          KM = KM + NUM7
#include "vectorize.inc"
          DO 960 I=GREFT,GRELT
           GRESAV(I)    = GRESAV(I) + GRESAV(I+KM)
           GRESAV(I+KM) = 0.
 960      CONTINUE
 970    CONTINUE
      ENDIF
      CALL MY_BARRIER
C
      RETURN
      END
