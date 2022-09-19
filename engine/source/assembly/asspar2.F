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
Chd|  ASSPAR2                       source/assembly/asspar2.F     
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ASS2SORT                      source/assembly/ass2sort.F    
Chd|        IDP_FREE                      source/system/machine.F       
Chd|        IDP_LOCK                      source/system/machine.F       
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ORDER_V                       source/assembly/asspar2.F     
Chd|====================================================================
      SUBROUTINE ASSPAR2(
     2   A          ,AR         ,ITASK      ,NODFT     ,
     3   NODLT      ,STIFN      ,STIFR      ,ITAB      ,FSKY      ,
     4   FSKYV      ,ISKY       ,INDSKY     ,FSKYI     ,
     5   ADSKYI     ,PARTFT     ,PARTLT     ,PARTSAV   ,VIND1     ,
     6   VIND2      ,VIND3      ,MS         ,FSKYM     ,
     7   MSNF       ,VISCN      ,FTHE       ,FTHESKY   , FTHESKYI ,
     8   STCND      ,AF         ,FFSKY      ,MSF       )
C----6---------------------------------------------------------------7---------8
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "couple_c.inc"
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "remesh_c.inc"
#include      "scr06_c.inc"
#include      "scr07_c.inc"
#include      "scr18_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
#include      "vale_c.inc"
#include      "warn_c.inc"
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
     .   A(3,*)  ,AR(3,*),STIFN(*),STIFR(*),FSKYV(LSKY,8),
     .   FSKYI(LSKYI,NFSKYI),PARTSAV(*),FSKY(8,LSKY), MS(*),
     .   FSKYM(*), MSNF(*), VISCN(*),FTHE(*), FTHESKY(LSKY),
     .   FTHESKYI(LSKYI),STCND(*),AF(3,*),FFSKY(3,*),
     .   MSF(*)
      INTEGER ITASK,NODFT,NODLT,PARTFT,PARTLT,
     .        VIND1(*),VIND2(*),VIND3(*)
      INTEGER ISKY(*),INDSKY(0:*),ADSKYI(0:*),ITAB(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER VSIZE, NBCC
      PARAMETER (VSIZE = 8192)
      PARAMETER (NBCC = 20)
      INTEGER I,J,K,L,N,KK,J1,J2,JJ1,JJ2,NN,KM,NUM7,
     .        NISKYFT,NISKYLT, LDONE,IDONE,NC,NL,IFT,ILT,KFT,KLT,
     .        K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,I0,IBAR,IJK,NEWN,PAS,
     .        NCT,DIFFADD,NDLT,NDFT,LMOD,LJ,KKEND,JJ,NSTART,
     .        KKSTART,KMAX,KSPLIT,IC(NBCC+1),NN_A(VSIZE),IARRAY(VSIZE),
     .        IVSIZE
Cel attention 21   = NBCC+1   == nb contact critiques + 1 (test qsort ci-dessous)
Cel attention NBCC+1 utilisee dans REALLY_ORDER_V
Cel attention 8192 = VSIZE  == taille vecteurs travail boucle interface
      my_real
     .        FF, FSKYT(NISKY)
      COMMON /ASSP2/ LDONE, IDONE, IBAR
C----------------------------------------------- 
      IF(NISKY>LSKYI)THEN
        WRITE(IOUT,*) ' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        WRITE(IOUT,*)
     .    ' PLEASE, INCREASE MULTIMP FOR INTERFACES 7, 10 AND 11'
        WRITE(ISTDO,*)' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        TSTOP=ZERO
        RETURN
      ENDIF
C test devenu inutile  : alloc auto en v50c

      NISKYFT   = 1+ITASK*NISKY/ NTHREAD
      NISKYLT   = (ITASK+1)*NISKY/NTHREAD
c      print *,' nisky=',nisky,' nodlt=',nodlt
C
      DO 100 N=NODFT,NODLT
        ADSKYI(N) = 0
 100  CONTINUE
      ADSKYI(NUMNOD+1) = 0
C
      IDONE = 0
      LDONE = 0
      IBAR  = 0
      INTDONE  = 1
      IF (IVECTOR==0) NBDONE(1)=0
      DO  L=1,NBLOC
       NBDONE(L) =  0
      ENDDO
      CALL MY_BARRIER
C-----------------------------------------------
C     FORCES D'INTERFACES
C-----------------------------------------------
      IF (NISKY/=0) THEN
       CALL IDP_LOCK(1)
       IF(IDONE/=0)THEN
         CALL IDP_FREE(1)
       ELSE
        IDONE = 1
        CALL IDP_FREE(1)
C
        DO 120 I=1,NISKY
          N = ISKY(I) +1
          ADSKYI(N) = ADSKYI(N) + 1
 120    CONTINUE
C-----------------------------------------------
C   CALCUL DES ADRESSES DU VECTEUR SKYLINE
C-----------------------------------------------
        ADSKYI(0) = 1
        ADSKYI(1) = 1
        DO 200 N=1,NUMNOD
          NN = N+1
          ADSKYI(NN) = ADSKYI(NN) + ADSKYI(N)
 200    CONTINUE
C-----------------------------------------------
C   TRI DES FORCES EN SKYLINE
C-----------------------------------------------
        DO 250 I=1,NISKY
          N = ISKY(I)
          J  = ADSKYI(N)
          ISKY(I)  = J
          ADSKYI(N) = ADSKYI(N) + 1
 250    CONTINUE
C
        CALL IDP_LOCK(2)
          IDONE = 2
        CALL IDP_FREE(2)
       ENDIF
      ELSE
       IDONE=2
      ENDIF
C-----------------------------------------------
C     FORCES D'ELEMENTS 
C-----------------------------------------------
      IF(IVECTOR==1) THEN
      I0=0
      DO  L=1,NBLOC
       LMOD=MOD(l,INTSEG)
       NC  = NBCOL(L)
       NL  = (ADBLOC(L+1)-ADBLOC(L))/NC
 300   CONTINUE
C
       CALL IDP_LOCK(7+LMOD)
         IFT = NBDONE(L)
         NBDONE(L) = IFT + NVSIZ
         IF(IFT>=NL)THEN
           CALL IDP_FREE(7+LMOD)
           GOTO 301
         ENDIF
       CALL IDP_FREE(7+LMOD)
C
       ILT = MIN(IFT+NVSIZ, NL) - 1
       I   = I0 + IFT
       KFT = ADBLOC(L) + IFT*NC
       KLT = ADBLOC(L) + ILT*NC
       IF(NC==1)THEN
         IF(ITHERM_FE == 0 ) THEN
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include    "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             A(1,N)   = A(1,N) +FSKYV(K1,1)
             A(2,N)   = A(2,N) +FSKYV(K1,2)
             A(3,N)   = A(3,N) +FSKYV(K1,3)
             AR(1,N)  = AR(1,N) +FSKYV(K1,4)
             AR(2,N)  = AR(2,N) +FSKYV(K1,5)
             AR(3,N)  = AR(3,N) +FSKYV(K1,6)
             STIFN(N) = STIFN(N) +FSKYV(K1,7)
             STIFR(N) = STIFR(N) +FSKYV(K1,8)
            ENDDO
           ELSE
#include    "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             A(1,N)   = A(1,N) +FSKYV(K1,1)
             A(2,N)   = A(2,N) +FSKYV(K1,2)
             A(3,N)   = A(3,N) +FSKYV(K1,3)
             AR(1,N)  = AR(1,N) +FSKYV(K1,4)
             AR(2,N)  = AR(2,N) +FSKYV(K1,5)
             AR(3,N)  = AR(3,N) +FSKYV(K1,6)
             STIFN(N) = STIFN(N) +FSKYV(K1,7)
             STIFR(N) = STIFR(N) +FSKYV(K1,8)
             MSNF(N)  = MSNF(N)  +FSKYM(K1)
            ENDDO
           ENDIF
C la thermique
          ELSE
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include    "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             A(1,N)   = A(1,N) +FSKYV(K1,1)
             A(2,N)   = A(2,N) +FSKYV(K1,2)
             A(3,N)   = A(3,N) +FSKYV(K1,3)
             AR(1,N)  = AR(1,N) +FSKYV(K1,4)
             AR(2,N)  = AR(2,N) +FSKYV(K1,5)
             AR(3,N)  = AR(3,N) +FSKYV(K1,6)
             STIFN(N) = STIFN(N) +FSKYV(K1,7)
             STIFR(N) = STIFR(N) +FSKYV(K1,8)
             FTHE(N)  = FTHE(N) + FTHESKY(K1)
            ENDDO
           ELSE
#include    "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             A(1,N)   = A(1,N) +FSKYV(K1,1)
             A(2,N)   = A(2,N) +FSKYV(K1,2)
             A(3,N)   = A(3,N) +FSKYV(K1,3)
             AR(1,N)  = AR(1,N) +FSKYV(K1,4)
             AR(2,N)  = AR(2,N) +FSKYV(K1,5)
             AR(3,N)  = AR(3,N) +FSKYV(K1,6)
             STIFN(N) = STIFN(N) +FSKYV(K1,7)
             STIFR(N) = STIFR(N) +FSKYV(K1,8)
             MSNF(N)  = MSNF(N)  +FSKYM(K1)
             FTHE(N)  = FTHE(N) + FTHESKY(K1)
            ENDDO
           ENDIF
         ENDIF
       ELSEIF(NC==2)THEN
        IF(ITHERM_FE == 0 ) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
         DO  K1=KFT,KLT,NC
          I=I+1
          N=INDSKY(I)
          K2=K1+1
          A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)
          A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)
          A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)
          AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)
          AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)
          AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)
          STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)
          STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)
         ENDDO
        ELSE
#include "vectorize.inc"
         DO  K1=KFT,KLT,NC
          I=I+1
          N=INDSKY(I)
          K2=K1+1
          A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)
          A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)
          A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)
          AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)
          AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)
          AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)
          STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)
          STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)
          MSNF(N)  = MSNF(N)
     .     +FSKYM(K1) +FSKYM(K2)
          ENDDO
         ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
         DO  K1=KFT,KLT,NC
          I=I+1
          N=INDSKY(I)
          K2=K1+1
          A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)
          A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)
          A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)
          AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)
          AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)
          AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)
          STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)
          STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)
           FTHE(N)   = FTHE(N) + FTHESKY(K1) + FTHESKY(K2)
         ENDDO
        ELSE
#include "vectorize.inc"
         DO  K1=KFT,KLT,NC
          I=I+1
          N=INDSKY(I)
          K2=K1+1
          A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)
          A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)
          A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)
          AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)
          AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)
          AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)
          STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)
          STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)
          MSNF(N)  = MSNF(N)
     .     +FSKYM(K1) +FSKYM(K2)
           FTHE(N)   = FTHE(N) + FTHESKY(K1) + FTHESKY(K2)
          ENDDO
         ENDIF
        ENDIF
       ELSEIF(NC==3)THEN
        IF(ITHERM_FE == 0 ) THEN
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             K2=K1+1
             K3=K1+2
             A(1,N)  = A(1,N)
     .        +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1)
             A(2,N)  = A(2,N)
     .        +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2)
             A(3,N)  = A(3,N)
     .        +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3)
             AR(1,N)  = AR(1,N)
     .        +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4)
             AR(2,N)  = AR(2,N)
     .        +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5)
             AR(3,N)  = AR(3,N)
     .        +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6)
             STIFN(N)  = STIFN(N)
     .        +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7)
             STIFR(N)  = STIFR(N)
     .        +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8)
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             K2=K1+1
             K3=K1+2
             A(1,N)  = A(1,N)
     .        +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1)
             A(2,N)  = A(2,N)
     .        +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2)
             A(3,N)  = A(3,N)
     .        +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3)
             AR(1,N)  = AR(1,N)
     .        +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4)
             AR(2,N)  = AR(2,N)
     .        +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5)
             AR(3,N)  = AR(3,N)
     .        +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6)
             STIFN(N)  = STIFN(N)
     .        +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7)
             STIFR(N)  = STIFR(N)
     .        +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8)
             MSNF(N)  = MSNF(N)
     .        +FSKYM(K1) +FSKYM(K2) +FSKYM(K3)
            ENDDO
           ENDIF
        ELSE
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             K2=K1+1
             K3=K1+2
             A(1,N)  = A(1,N)
     .        +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1)
             A(2,N)  = A(2,N)
     .        +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2)
             A(3,N)  = A(3,N)
     .        +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3)
             AR(1,N)  = AR(1,N)
     .        +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4)
             AR(2,N)  = AR(2,N)
     .        +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5)
             AR(3,N)  = AR(3,N)
     .        +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6)
             STIFN(N)  = STIFN(N)
     .        +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7)
             STIFR(N)  = STIFR(N)
     .        +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8)
              FTHE(N)   = FTHE(N) + FTHESKY(K1) + FTHESKY(K2)
     .                            + FTHESKY(K3)
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             K2=K1+1
             K3=K1+2
             A(1,N)  = A(1,N)
     .        +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1)
             A(2,N)  = A(2,N)
     .        +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2)
             A(3,N)  = A(3,N)
     .        +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3)
             AR(1,N)  = AR(1,N)
     .        +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4)
             AR(2,N)  = AR(2,N)
     .        +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5)
             AR(3,N)  = AR(3,N)
     .        +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6)
             STIFN(N)  = STIFN(N)
     .        +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7)
             STIFR(N)  = STIFR(N)
     .        +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8)
             MSNF(N)  = MSNF(N)
     .        +FSKYM(K1) +FSKYM(K2) +FSKYM(K3)
             FTHE(N)   = FTHE(N) + FTHESKY(K1) + FTHESKY(K2)
     .                           + FTHESKY(K3)
            ENDDO
           ENDIF

        ENDIF
       ELSEIF(NC==4)THEN
        IF(ITHERM_FE == 0 ) THEN
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              K2=K1+1
              K3=K1+2
              K4=K1+3
              K5=K1+4
              A(1,N)  = A(1,N)
     .         +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1) +FSKYV(K4,1)
              A(2,N)  = A(2,N)
     .         +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2) +FSKYV(K4,2)
              A(3,N)  = A(3,N)
     .         +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3) +FSKYV(K4,3)
              AR(1,N)  = AR(1,N)
     .         +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4) +FSKYV(K4,4)
              AR(2,N)  = AR(2,N)
     .         +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5) +FSKYV(K4,5)
              AR(3,N)  = AR(3,N)
     .         +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6) +FSKYV(K4,6)
              STIFN(N)  = STIFN(N)
     .         +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7) +FSKYV(K4,7)
              STIFR(N)  = STIFR(N)
     .         +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8) +FSKYV(K4,8)
             ENDDO
            ELSE
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              K2=K1+1
              K3=K1+2
              K4=K1+3
              K5=K1+4
              A(1,N)  = A(1,N)
     .         +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1) +FSKYV(K4,1)
              A(2,N)  = A(2,N)
     .         +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2) +FSKYV(K4,2)
              A(3,N)  = A(3,N)
     .         +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3) +FSKYV(K4,3)
              AR(1,N)  = AR(1,N)
     .         +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4) +FSKYV(K4,4)
              AR(2,N)  = AR(2,N)
     .         +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5) +FSKYV(K4,5)
              AR(3,N)  = AR(3,N)
     .         +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6) +FSKYV(K4,6)
              STIFN(N)  = STIFN(N)
     .         +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7) +FSKYV(K4,7)
              STIFR(N)  = STIFR(N)
     .         +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8) +FSKYV(K4,8)
              MSNF(N)  = MSNF(N)
     .         +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4)
             ENDDO
            ENDIF
         ELSE
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              K2=K1+1
              K3=K1+2
              K4=K1+3
              K5=K1+4
              A(1,N)  = A(1,N)
     .         +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1) +FSKYV(K4,1)
              A(2,N)  = A(2,N)
     .         +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2) +FSKYV(K4,2)
              A(3,N)  = A(3,N)
     .         +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3) +FSKYV(K4,3)
              AR(1,N)  = AR(1,N)
     .         +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4) +FSKYV(K4,4)
              AR(2,N)  = AR(2,N)
     .         +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5) +FSKYV(K4,5)
              AR(3,N)  = AR(3,N)
     .         +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6) +FSKYV(K4,6)
              STIFN(N)  = STIFN(N)
     .         +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7) +FSKYV(K4,7)
              STIFR(N)  = STIFR(N)
     .         +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8) +FSKYV(K4,8)
                FTHE(N)   = FTHE(N)    + FTHESKY(K1) + FTHESKY(K2)
     .                                 + FTHESKY(K3) + FTHESKY(K4)
             ENDDO
            ELSE
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              K2=K1+1
              K3=K1+2
              K4=K1+3
              K5=K1+4
              A(1,N)  = A(1,N)
     .         +FSKYV(K1,1) +FSKYV(K2,1) +FSKYV(K3,1) +FSKYV(K4,1)
              A(2,N)  = A(2,N)
     .         +FSKYV(K1,2) +FSKYV(K2,2) +FSKYV(K3,2) +FSKYV(K4,2)
              A(3,N)  = A(3,N)
     .         +FSKYV(K1,3) +FSKYV(K2,3) +FSKYV(K3,3) +FSKYV(K4,3)
              AR(1,N)  = AR(1,N)
     .         +FSKYV(K1,4) +FSKYV(K2,4) +FSKYV(K3,4) +FSKYV(K4,4)
              AR(2,N)  = AR(2,N)
     .         +FSKYV(K1,5) +FSKYV(K2,5) +FSKYV(K3,5) +FSKYV(K4,5)
              AR(3,N)  = AR(3,N)
     .         +FSKYV(K1,6) +FSKYV(K2,6) +FSKYV(K3,6) +FSKYV(K4,6)
              STIFN(N)  = STIFN(N)
     .         +FSKYV(K1,7) +FSKYV(K2,7) +FSKYV(K3,7) +FSKYV(K4,7)
              STIFR(N)  = STIFR(N)
     .         +FSKYV(K1,8) +FSKYV(K2,8) +FSKYV(K3,8) +FSKYV(K4,8)
              MSNF(N)  = MSNF(N)
     .         +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4)
               FTHE(N)   = FTHE(N)    + FTHESKY(K1) + FTHESKY(K2)
     .                                + FTHESKY(K3) + FTHESKY(K4)
             ENDDO
            ENDIF
         ENDIF
C
       ELSEIF(NC==5)THEN
        IF(ITHERM_FE == 0 ) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
           AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
           AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
           AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
           STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
           STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
          ENDDO
         ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
            FTHE(N)   = FTHE(N)     +  FTHESKY(K1) + FTHESKY(K2)
     .                              +  FTHESKY(K3) + FTHESKY(K4)
     .                              +  FTHESKY(K5)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
            FTHE(N)   = FTHE(N)     +  FTHESKY(K1) + FTHESKY(K2)
     .                              +  FTHESKY(K3) + FTHESKY(K4)
     .                              +  FTHESKY(K5)
          ENDDO
         ENDIF

        ENDIF
       ELSEIF(NC==6)THEN
        IF(ITHERM_FE == 0) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)
          ENDDO
         ELSE
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6)
           ENDDO
          ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)
              FTHE(N)   = FTHE(N)     +  FTHESKY(K1) + FTHESKY(K2)
     .                                +  FTHESKY(K3) + FTHESKY(K4)
     .                                +  FTHESKY(K5) + FTHESKY(K6)
          ENDDO
         ELSE
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6)
            FTHE(N)   = FTHE(N) + FTHESKY(K1) + FTHESKY(K2)
     .                          + FTHESKY(K3) + FTHESKY(K4)
     .                          + FTHESKY(K5) + FTHESKY(K6)
           ENDDO
          ENDIF
        ENDIF
       ELSEIF(NC==7)THEN
        IF(ITHERM_FE == 0 ) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7)
          ENDDO
         ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8)
            FTHE(N)   = FTHE(N) +  FTHESKY(K1) + FTHESKY(K2)
     .                          +  FTHESKY(K3) + FTHESKY(K4)
     .                          +  FTHESKY(K5) + FTHESKY(K6)
     .                          +  FTHESKY(K7)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7)
            FTHE(N)   = FTHE(N) +  FTHESKY(K1) + FTHESKY(K2)
     .                          +  FTHESKY(K3) + FTHESKY(K4)
     .                          +  FTHESKY(K5) + FTHESKY(K6)
     .                          +  FTHESKY(K7)
          ENDDO
         ENDIF
        ENDIF
       ELSEIF(NC==8)THEN
        IF(ITHERM_FE == 0 ) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7) +FSKYM(K8)
          ENDDO
         ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8)
              FTHE(N)   = FTHE(N) +  FTHESKY(K1) + FTHESKY(K2)
     .                            +  FTHESKY(K3) + FTHESKY(K4)
     .                            +  FTHESKY(K5) + FTHESKY(K6)
     .                            +  FTHESKY(K7) + FTHESKY(K8)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7) +FSKYM(K8)
              FTHE(N)   = FTHE(N) +  FTHESKY(K1) + FTHESKY(K2)
     .                            +  FTHESKY(K3) + FTHESKY(K4)
     .                            +  FTHESKY(K5) + FTHESKY(K6)
     .                            +  FTHESKY(K7) + FTHESKY(K8)
          ENDDO
         ENDIF
        ENDIF
       ELSEIF(NC==9)THEN
        IF(ITHERM_FE == 0 ) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1) +FSKYV(K9,1)
           A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2) +FSKYV(K9,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3) +FSKYV(K9,3)
           AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4) +FSKYV(K9,4)
           AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5) +FSKYV(K9,5)
           AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6) +FSKYV(K9,6)
           STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7) +FSKYV(K9,7)
           STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8) +FSKYV(K9,8)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .     +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1) +FSKYV(K9,1)
           A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .     +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2) +FSKYV(K9,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .     +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3) +FSKYV(K9,3)
           AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4) +FSKYV(K9,4)
           AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5) +FSKYV(K9,5)
           AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6) +FSKYV(K9,6)
           STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7) +FSKYV(K9,7)
           STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8) +FSKYV(K9,8)
           MSNF(N)  = MSNF(N)
     .     +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7) +FSKYM(K8) +FSKYM(K9)
          ENDDO
         ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1) +FSKYV(K9,1)
           A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2) +FSKYV(K9,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3) +FSKYV(K9,3)
           AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4) +FSKYV(K9,4)
           AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5) +FSKYV(K9,5)
           AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6) +FSKYV(K9,6)
           STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7) +FSKYV(K9,7)
           STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8) +FSKYV(K9,8)
             FTHE(N)   = FTHE(N) + FTHESKY(K1) + FTHESKY(K2)
     .                           + FTHESKY(K3) + FTHESKY(K4)
     .                           + FTHESKY(K5) + FTHESKY(K6)
     .                           + FTHESKY(K7) + FTHESKY(K8)
     .                           + FTHESKY(K9)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           A(1,N)  = A(1,N)
     .     +FSKYV(K1,1) +FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .     +FSKYV(K6,1) +FSKYV(K7,1) +FSKYV(K8,1) +FSKYV(K9,1)
           A(2,N)  = A(2,N)
     .     +FSKYV(K1,2) +FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .     +FSKYV(K6,2) +FSKYV(K7,2) +FSKYV(K8,2) +FSKYV(K9,2)
           A(3,N)  = A(3,N)
     .     +FSKYV(K1,3) +FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .     +FSKYV(K6,3) +FSKYV(K7,3) +FSKYV(K8,3) +FSKYV(K9,3)
           AR(1,N)  = AR(1,N)
     .     +FSKYV(K1,4) +FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4) +FSKYV(K7,4) +FSKYV(K8,4) +FSKYV(K9,4)
           AR(2,N)  = AR(2,N)
     .     +FSKYV(K1,5) +FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5) +FSKYV(K7,5) +FSKYV(K8,5) +FSKYV(K9,5)
           AR(3,N)  = AR(3,N)
     .     +FSKYV(K1,6) +FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6) +FSKYV(K7,6) +FSKYV(K8,6) +FSKYV(K9,6)
           STIFN(N)  = STIFN(N)
     .     +FSKYV(K1,7) +FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7) +FSKYV(K7,7) +FSKYV(K8,7) +FSKYV(K9,7)
           STIFR(N)  = STIFR(N)
     .     +FSKYV(K1,8) +FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8) +FSKYV(K7,8) +FSKYV(K8,8) +FSKYV(K9,8)
           MSNF(N)  = MSNF(N)
     .     +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7) +FSKYM(K8) +FSKYM(K9)
             FTHE(N)   = FTHE(N) + FTHESKY(K1) + FTHESKY(K2)
     .                           + FTHESKY(K3) + FTHESKY(K4)
     .                           + FTHESKY(K5) + FTHESKY(K6)
     .                           + FTHESKY(K7) + FTHESKY(K8)
     .                           + FTHESKY(K9)
          ENDDO
         ENDIF

        ENDIF
       ELSEIF(NC==10)THEN
        IF(ITHERM_FE == 0 ) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           K0=K1+9
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)+FSKYV(K7,1)+FSKYV(K8,1)+FSKYV(K9,1)+FSKYV(K0,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)+FSKYV(K7,2)+FSKYV(K8,2)+FSKYV(K9,2)+FSKYV(K0,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)+FSKYV(K7,3)+FSKYV(K8,3)+FSKYV(K9,3)+FSKYV(K0,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)+FSKYV(K7,4)+FSKYV(K8,4)+FSKYV(K9,4)+FSKYV(K0,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)+FSKYV(K7,5)+FSKYV(K8,5)+FSKYV(K9,5)+FSKYV(K0,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)+FSKYV(K7,6)+FSKYV(K8,6)+FSKYV(K9,6)+FSKYV(K0,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)+FSKYV(K7,7)+FSKYV(K8,7)+FSKYV(K9,7)+FSKYV(K0,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)+FSKYV(K7,8)+FSKYV(K8,8)+FSKYV(K9,8)+FSKYV(K0,8)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           K0=K1+9
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)+FSKYV(K7,1)+FSKYV(K8,1)+FSKYV(K9,1)+FSKYV(K0,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)+FSKYV(K7,2)+FSKYV(K8,2)+FSKYV(K9,2)+FSKYV(K0,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)+FSKYV(K7,3)+FSKYV(K8,3)+FSKYV(K9,3)+FSKYV(K0,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)+FSKYV(K7,4)+FSKYV(K8,4)+FSKYV(K9,4)+FSKYV(K0,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)+FSKYV(K7,5)+FSKYV(K8,5)+FSKYV(K9,5)+FSKYV(K0,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)+FSKYV(K7,6)+FSKYV(K8,6)+FSKYV(K9,6)+FSKYV(K0,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)+FSKYV(K7,7)+FSKYV(K8,7)+FSKYV(K9,7)+FSKYV(K0,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)+FSKYV(K7,8)+FSKYV(K8,8)+FSKYV(K9,8)+FSKYV(K0,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7) +FSKYM(K8) +FSKYM(K9) +FSKYM(K0)
          ENDDO
         ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           K0=K1+9
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)+FSKYV(K7,1)+FSKYV(K8,1)+FSKYV(K9,1)+FSKYV(K0,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)+FSKYV(K7,2)+FSKYV(K8,2)+FSKYV(K9,2)+FSKYV(K0,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)+FSKYV(K7,3)+FSKYV(K8,3)+FSKYV(K9,3)+FSKYV(K0,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)+FSKYV(K7,4)+FSKYV(K8,4)+FSKYV(K9,4)+FSKYV(K0,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)+FSKYV(K7,5)+FSKYV(K8,5)+FSKYV(K9,5)+FSKYV(K0,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)+FSKYV(K7,6)+FSKYV(K8,6)+FSKYV(K9,6)+FSKYV(K0,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)+FSKYV(K7,7)+FSKYV(K8,7)+FSKYV(K9,7)+FSKYV(K0,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)+FSKYV(K7,8)+FSKYV(K8,8)+FSKYV(K9,8)+FSKYV(K0,8)
            FTHE(N)   = FTHE(N)  + FTHESKY(K1) + FTHESKY(K2)
     .                           + FTHESKY(K3) + FTHESKY(K4)
     .                           + FTHESKY(K5) + FTHESKY(K6)
     .                           + FTHESKY(K7) + FTHESKY(K8)
     .                           + FTHESKY(K9) + FTHESKY(K0)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           K2=K1+1
           K3=K1+2
           K4=K1+3
           K5=K1+4
           K6=K1+5
           K7=K1+6
           K8=K1+7
           K9=K1+8
           K0=K1+9
           A(1,N)  = A(1,N)
     .      +FSKYV(K1,1)+FSKYV(K2,1)+FSKYV(K3,1)+FSKYV(K4,1)+FSKYV(K5,1)
     .      +FSKYV(K6,1)+FSKYV(K7,1)+FSKYV(K8,1)+FSKYV(K9,1)+FSKYV(K0,1)
           A(2,N)  = A(2,N)
     .      +FSKYV(K1,2)+FSKYV(K2,2)+FSKYV(K3,2)+FSKYV(K4,2)+FSKYV(K5,2)
     .      +FSKYV(K6,2)+FSKYV(K7,2)+FSKYV(K8,2)+FSKYV(K9,2)+FSKYV(K0,2)
           A(3,N)  = A(3,N)
     .      +FSKYV(K1,3)+FSKYV(K2,3)+FSKYV(K3,3)+FSKYV(K4,3)+FSKYV(K5,3)
     .      +FSKYV(K6,3)+FSKYV(K7,3)+FSKYV(K8,3)+FSKYV(K9,3)+FSKYV(K0,3)
           AR(1,N)  = AR(1,N)
     .      +FSKYV(K1,4)+FSKYV(K2,4)+FSKYV(K3,4)+FSKYV(K4,4)+FSKYV(K5,4)
     .      +FSKYV(K6,4)+FSKYV(K7,4)+FSKYV(K8,4)+FSKYV(K9,4)+FSKYV(K0,4)
           AR(2,N)  = AR(2,N)
     .      +FSKYV(K1,5)+FSKYV(K2,5)+FSKYV(K3,5)+FSKYV(K4,5)+FSKYV(K5,5)
     .      +FSKYV(K6,5)+FSKYV(K7,5)+FSKYV(K8,5)+FSKYV(K9,5)+FSKYV(K0,5)
           AR(3,N)  = AR(3,N)
     .      +FSKYV(K1,6)+FSKYV(K2,6)+FSKYV(K3,6)+FSKYV(K4,6)+FSKYV(K5,6)
     .      +FSKYV(K6,6)+FSKYV(K7,6)+FSKYV(K8,6)+FSKYV(K9,6)+FSKYV(K0,6)
           STIFN(N)  = STIFN(N)
     .      +FSKYV(K1,7)+FSKYV(K2,7)+FSKYV(K3,7)+FSKYV(K4,7)+FSKYV(K5,7)
     .      +FSKYV(K6,7)+FSKYV(K7,7)+FSKYV(K8,7)+FSKYV(K9,7)+FSKYV(K0,7)
           STIFR(N)  = STIFR(N)
     .      +FSKYV(K1,8)+FSKYV(K2,8)+FSKYV(K3,8)+FSKYV(K4,8)+FSKYV(K5,8)
     .      +FSKYV(K6,8)+FSKYV(K7,8)+FSKYV(K8,8)+FSKYV(K9,8)+FSKYV(K0,8)
           MSNF(N)  = MSNF(N)
     .      +FSKYM(K1) +FSKYM(K2) +FSKYM(K3) +FSKYM(K4) +FSKYM(K5)
     .      +FSKYM(K6) +FSKYM(K7) +FSKYM(K8) +FSKYM(K9) +FSKYM(K0)
            FTHE(N)   = FTHE(N)  + FTHESKY(K1) + FTHESKY(K2)
     .                           + FTHESKY(K3) + FTHESKY(K4)
     .                           + FTHESKY(K5) + FTHESKY(K6)
     .                           + FTHESKY(K7) + FTHESKY(K8)
     .                           + FTHESKY(K9) + FTHESKY(K0)
          ENDDO
         ENDIF
        ENDIF
       ELSEIF (NC==11) THEN
        IF(ITHERM_FE == 0) THEN
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,11
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
            ENDDO
           ENDDO
          ELSE
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,11
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
            ENDDO
           ENDDO
          ENDIF
         ELSE
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,11
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ELSE
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,11
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ENDIF

         ENDIF
       ELSEIF (NC==12) THEN
        IF(ITHERM_FE == 0 ) THEN
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,12
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
            ENDDO
           ENDDO
          ELSE
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,12
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
            ENDDO
           ENDDO
          ENDIF
        ELSE
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,12
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ELSE
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,12
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ENDIF
        ENDIF
       ELSEIF (NC==13) THEN
        IF(ITHERM_FE == 0 ) THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           DO K2=1,13
            K = K1+K2-1
            A(1,N)   = A(1,N)   + FSKYV(K,1)
            A(2,N)   = A(2,N)   + FSKYV(K,2)
            A(3,N)   = A(3,N)   + FSKYV(K,3)
            AR(1,N)  = AR(1,N)  + FSKYV(K,4)
            AR(2,N)  = AR(2,N)  + FSKYV(K,5)
            AR(3,N)  = AR(3,N)  + FSKYV(K,6)
            STIFN(N) = STIFN(N) + FSKYV(K,7)
            STIFR(N) = STIFR(N) + FSKYV(K,8)
           ENDDO
          ENDDO
         ELSE
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           DO K2=1,13
            K = K1+K2-1
            A(1,N)   = A(1,N)   + FSKYV(K,1)
            A(2,N)   = A(2,N)   + FSKYV(K,2)
            A(3,N)   = A(3,N)   + FSKYV(K,3)
            AR(1,N)  = AR(1,N)  + FSKYV(K,4)
            AR(2,N)  = AR(2,N)  + FSKYV(K,5)
            AR(3,N)  = AR(3,N)  + FSKYV(K,6)
            STIFN(N) = STIFN(N) + FSKYV(K,7)
            STIFR(N) = STIFR(N) + FSKYV(K,8)
            MSNF(N)  = MSNF(N)  + FSKYM(K)
           ENDDO
          ENDDO
         ENDIF
        ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           DO K2=1,13
            K = K1+K2-1
            A(1,N)   = A(1,N)   + FSKYV(K,1)
            A(2,N)   = A(2,N)   + FSKYV(K,2)
            A(3,N)   = A(3,N)   + FSKYV(K,3)
            AR(1,N)  = AR(1,N)  + FSKYV(K,4)
            AR(2,N)  = AR(2,N)  + FSKYV(K,5)
            AR(3,N)  = AR(3,N)  + FSKYV(K,6)
            STIFN(N) = STIFN(N) + FSKYV(K,7)
            STIFR(N) = STIFR(N) + FSKYV(K,8)
            FTHE(N)   = FTHE(N) + FTHESKY(K)
           ENDDO
          ENDDO
         ELSE
#include  "vectorize.inc"
          DO  K1=KFT,KLT,NC
           I=I+1
           N=INDSKY(I)
           DO K2=1,13
            K = K1+K2-1
            A(1,N)   = A(1,N)   + FSKYV(K,1)
            A(2,N)   = A(2,N)   + FSKYV(K,2)
            A(3,N)   = A(3,N)   + FSKYV(K,3)
            AR(1,N)  = AR(1,N)  + FSKYV(K,4)
            AR(2,N)  = AR(2,N)  + FSKYV(K,5)
            AR(3,N)  = AR(3,N)  + FSKYV(K,6)
            STIFN(N) = STIFN(N) + FSKYV(K,7)
            STIFR(N) = STIFR(N) + FSKYV(K,8)
            MSNF(N)  = MSNF(N)  + FSKYM(K)
            FTHE(N)   = FTHE(N) + FTHESKY(K)
           ENDDO
          ENDDO
         ENDIF
        ENDIF
       ELSEIF (NC==14) THEN
        IF(ITHERM_FE == 0) THEN
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,14
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
             ENDDO
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,14
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              MSNF(N)  = MSNF(N)  + FSKYM(K)
             ENDDO
            ENDDO
           ENDIF
        ELSE
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,14
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              FTHE(N)  = FTHE(N)  + FTHESKY(K)
             ENDDO
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,14
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              MSNF(N)  = MSNF(N)  + FSKYM(K)
              FTHE(N)  = FTHE(N)  + FTHESKY(K)
             ENDDO
            ENDDO
           ENDIF

        ENDIF
       ELSEIF (NC==15) THEN
        IF(ITHERM_FE == 0 ) THEN
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,15
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
             ENDDO
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,15
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              MSNF(N)  = MSNF(N)  + FSKYM(K)
             ENDDO
            ENDDO
           ENDIF
        ELSE
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,15
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              FTHE(N)  = FTHE(N)  + FTHESKY(K)
             ENDDO
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,15
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              MSNF(N)  = MSNF(N)  + FSKYM(K)
              FTHE(N)  = FTHE(N)  + FTHESKY(K)
             ENDDO
            ENDDO
           ENDIF
        ENDIF
       ELSEIF (NC==16) THEN
        IF(ITHERM_FE == 0 ) THEN
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,16
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
             ENDDO
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,16
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              MSNF(N)  = MSNF(N)  + FSKYM(K)
             ENDDO
            ENDDO
           ENDIF
         ELSE
           IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,16
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              FTHE(N)   = FTHE(N) + FTHESKY(K)
             ENDDO
            ENDDO
           ELSE
#include "vectorize.inc"
            DO  K1=KFT,KLT,NC
             I=I+1
             N=INDSKY(I)
             DO K2=1,16
              K = K1+K2-1
              A(1,N)   = A(1,N)   + FSKYV(K,1)
              A(2,N)   = A(2,N)   + FSKYV(K,2)
              A(3,N)   = A(3,N)   + FSKYV(K,3)
              AR(1,N)  = AR(1,N)  + FSKYV(K,4)
              AR(2,N)  = AR(2,N)  + FSKYV(K,5)
              AR(3,N)  = AR(3,N)  + FSKYV(K,6)
              STIFN(N) = STIFN(N) + FSKYV(K,7)
              STIFR(N) = STIFR(N) + FSKYV(K,8)
              MSNF(N)  = MSNF(N)  + FSKYM(K)
              FTHE(N)   = FTHE(N) + FTHESKY(K)
             ENDDO
            ENDDO
           ENDIF
         ENDIF
       ELSEIF (NC==17) THEN
        IF(ITHERM_FE == 0) THEN
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,17
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
            ENDDO
           ENDDO
          ELSE
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,17
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
            ENDDO
           ENDDO
          ENDIF
        ELSE
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,17
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ELSE
#include   "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,17
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ENDIF
        ENDIF
       ELSEIF (NC==18) THEN
        IF(ITHERM_FE == 0 ) THEN
            IF(N2D/=0.OR.IFSUBM==0)THEN
#include  "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,18
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
              ENDDO
             ENDDO
            ELSE
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,18
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
               MSNF(N)  = MSNF(N)  + FSKYM(K)
              ENDDO
             ENDDO
            ENDIF
        ELSE
            IF(N2D/=0.OR.IFSUBM==0)THEN
#include  "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,18
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
               FTHE(N)   = FTHE(N) + FTHESKY(K)
              ENDDO
             ENDDO
            ELSE
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,18
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
               MSNF(N)  = MSNF(N)  + FSKYM(K)
               FTHE(N)   = FTHE(N) + FTHESKY(K)
              ENDDO
             ENDDO
            ENDIF
        ENDIF
       ELSEIF (NC==19) THEN
        IF(ITHERM_FE == 0 ) THEN
            IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,19
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
              ENDDO
             ENDDO
            ELSE
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,19
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
               MSNF(N)  = MSNF(N)  + FSKYM(K)
              ENDDO
             ENDDO
            ENDIF
        ELSE
             IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,19
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
               FTHE(N)   = FTHE(N) + FTHESKY(K)
              ENDDO
             ENDDO
            ELSE
#include "vectorize.inc"
             DO  K1=KFT,KLT,NC
              I=I+1
              N=INDSKY(I)
              DO K2=1,19
               K = K1+K2-1
               A(1,N)   = A(1,N)   + FSKYV(K,1)
               A(2,N)   = A(2,N)   + FSKYV(K,2)
               A(3,N)   = A(3,N)   + FSKYV(K,3)
               AR(1,N)  = AR(1,N)  + FSKYV(K,4)
               AR(2,N)  = AR(2,N)  + FSKYV(K,5)
               AR(3,N)  = AR(3,N)  + FSKYV(K,6)
               STIFN(N) = STIFN(N) + FSKYV(K,7)
               STIFR(N) = STIFR(N) + FSKYV(K,8)
               MSNF(N)  = MSNF(N)  + FSKYM(K)
               FTHE(N)   = FTHE(N) + FTHESKY(K)
              ENDDO
             ENDDO
            ENDIF
        ENDIF
       ELSEIF (NC==20) THEN
        IF(ITHERM_FE == 0) THEN
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,20
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
            ENDDO
           ENDDO
          ELSE
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,20
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
            ENDDO
           ENDDO
          ENDIF
        ELSE
          IF(N2D/=0.OR.IFSUBM==0)THEN
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,20
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ELSE
#include "vectorize.inc"
           DO  K1=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K2=1,20
             K = K1+K2-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ENDIF
        ENDIF
       ELSE
        IF(ITHERM_FE == 0 ) THEN
          IF(N2D/=0.OR.IFSUBM==0)THEN
           DO  KK=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K=KK,KK+NC-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
            ENDDO
           ENDDO
          ELSE
           DO  KK=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K=KK,KK+NC-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
            ENDDO
           ENDDO
          ENDIF
         ELSE
          IF(N2D/=0.OR.IFSUBM==0)THEN
           DO  KK=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K=KK,KK+NC-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             FTHE(N)  = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ELSE
           DO  KK=KFT,KLT,NC
            I=I+1
            N=INDSKY(I)
            DO K=KK,KK+NC-1
             A(1,N)   = A(1,N)   + FSKYV(K,1)
             A(2,N)   = A(2,N)   + FSKYV(K,2)
             A(3,N)   = A(3,N)   + FSKYV(K,3)
             AR(1,N)  = AR(1,N)  + FSKYV(K,4)
             AR(2,N)  = AR(2,N)  + FSKYV(K,5)
             AR(3,N)  = AR(3,N)  + FSKYV(K,6)
             STIFN(N) = STIFN(N) + FSKYV(K,7)
             STIFR(N) = STIFR(N) + FSKYV(K,8)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
             FTHE(N)  = FTHE(N) + FTHESKY(K)
            ENDDO
           ENDDO
          ENDIF
         ENDIF
       ENDIF
       GOTO 300
 301   CONTINUE
       I0 = I0 + NL
      ENDDO
      ELSE
C else ivector machine scalaire
       IF(ITHERM_FE == 0 )THEN
         IF(N2D/=0.OR.IFSUBM==0)THEN
          DO N = IDEB(ITASK+1), IFIN(ITASK+1)
           NCT = INDSKY(N)-1
           NC = INDSKY(N+1)-INDSKY(N)
           DO K = NCT+1, NCT+NC
             A(1,N)   = A(1,N)   + FSKY(1,K)
             A(2,N)   = A(2,N)   + FSKY(2,K)
             A(3,N)   = A(3,N)   + FSKY(3,K)
             AR(1,N)  = AR(1,N)  + FSKY(4,K)
             AR(2,N)  = AR(2,N)  + FSKY(5,K)
             AR(3,N)  = AR(3,N)  + FSKY(6,K)
             STIFN(N) = STIFN(N) + FSKY(7,K)
             STIFR(N) = STIFR(N) + FSKY(8,K)
           ENDDO
          ENDDO
         ELSE
          DO N = IDEB(ITASK+1), IFIN(ITASK+1)
           NCT = INDSKY(N)-1
           NC = INDSKY(N+1)-INDSKY(N)
           DO K = NCT+1, NCT+NC
             A(1,N)   = A(1,N)   + FSKY(1,K)
             A(2,N)   = A(2,N)   + FSKY(2,K)
             A(3,N)   = A(3,N)   + FSKY(3,K)
             AR(1,N)  = AR(1,N)  + FSKY(4,K)
             AR(2,N)  = AR(2,N)  + FSKY(5,K)
             AR(3,N)  = AR(3,N)  + FSKY(6,K)
             STIFN(N) = STIFN(N) + FSKY(7,K)
             STIFR(N) = STIFR(N) + FSKY(8,K)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
           ENDDO
          ENDDO
         ENDIF
       ELSE
         IF(N2D/=0.OR.IFSUBM==0)THEN
          DO N = IDEB(ITASK+1), IFIN(ITASK+1)
           NCT = INDSKY(N)-1
           NC = INDSKY(N+1)-INDSKY(N)
           DO K = NCT+1, NCT+NC
             A(1,N)   = A(1,N)   + FSKY(1,K)
             A(2,N)   = A(2,N)   + FSKY(2,K)
             A(3,N)   = A(3,N)   + FSKY(3,K)
             AR(1,N)  = AR(1,N)  + FSKY(4,K)
             AR(2,N)  = AR(2,N)  + FSKY(5,K)
             AR(3,N)  = AR(3,N)  + FSKY(6,K)
             STIFN(N) = STIFN(N) + FSKY(7,K)
             STIFR(N) = STIFR(N) + FSKY(8,K)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
           ENDDO
          ENDDO
         ELSE
          DO N = IDEB(ITASK+1), IFIN(ITASK+1)
           NCT = INDSKY(N)-1
           NC = INDSKY(N+1)-INDSKY(N)
           DO K = NCT+1, NCT+NC
             A(1,N)   = A(1,N)   + FSKY(1,K)
             A(2,N)   = A(2,N)   + FSKY(2,K)
             A(3,N)   = A(3,N)   + FSKY(3,K)
             AR(1,N)  = AR(1,N)  + FSKY(4,K)
             AR(2,N)  = AR(2,N)  + FSKY(5,K)
             AR(3,N)  = AR(3,N)  + FSKY(6,K)
             STIFN(N) = STIFN(N) + FSKY(7,K)
             STIFR(N) = STIFR(N) + FSKY(8,K)
             MSNF(N)  = MSNF(N)  + FSKYM(K)
             FTHE(N)   = FTHE(N) + FTHESKY(K)
           ENDDO
          ENDDO
         ENDIF
       ENDIF
      ENDIF
C
C  simplified ale flow
C     
        IF(IALELAG > 0) THEN
          DO N = IDEB(ITASK+1), IFIN(ITASK+1)
           NCT = INDSKY(N)-1
           NC = INDSKY(N+1)-INDSKY(N)
           DO K = NCT+1, NCT+NC
             AF(1,N)   = AF(1,N)   + FFSKY(1,K)
             AF(2,N)   = AF(2,N)   + FFSKY(2,K)
             AF(3,N)   = AF(3,N)   + FFSKY(3,K)
             MSNF(N)   = MSNF(N)   + FSKYM(K)
           ENDDO
          ENDDO
        ENDIF
      IF(N2D/=0) THEN
        CALL MY_BARRIER
           DO I = NODFT, NODLT
             MS(I) = A(1,I)
             A(1,I) = ZERO
           ENDDO
        CALL MY_BARRIER
      ELSEIF(IFSUBM==1)THEN
        CALL MY_BARRIER
        DO I = NODFT, NODLT
          MS(I) = MS(I) + MSNF(I)
        ENDDO
      ENDIF
      IF(IALELAG > 0) THEN
       CALL MY_BARRIER
        DO I = NODFT, NODLT
          MSF(I) = MSF(I) + MSNF(I)
        ENDDO
      ENDIF
C-----------------------------------------------
C     LE CALL BARRIER EST FAIT SUR TOUS LES PROCES. 
C     SI ET SEULEMENT SI LA TACHE IDONE N'EST PAS FAITE 
C     QUAND LE PREMIER DES PROCES. ARRIVE ICI
C-----------------------------------------------
      IF (NISKY/=0) THEN
       CALL IDP_LOCK(4)
        IF(IDONE/=2)IBAR = 1
       CALL IDP_FREE(4)
       IF(IBAR==1)CALL MY_BARRIER
C-----------------------------------------------
 400   CONTINUE
       CALL IDP_LOCK(5)
       IF(LDONE>NFSKYI-1)THEN
        CALL IDP_FREE(5)
        GO TO 401
       ELSE
        LDONE = LDONE + 1
        L = LDONE
        CALL IDP_FREE(5)
        DO 320 I=1,NISKY
          J = ISKY(I)
          FSKYT(J) = FSKYI(I,L)
 320    CONTINUE
        DO 340 I=1,NISKY
          FSKYI(I,L) = FSKYT(I)
 340    CONTINUE
C
        IF(INTHEAT > 0 .AND. L == 1) THEN
          DO  I=1,NISKY
           J = ISKY(I)
           FSKYT(J) = FTHESKYI(I)
          ENDDO
          DO  I=1,NISKY
            FTHESKYI(I) = FSKYT(I)
          ENDDO
        ENDIF
C
        GOTO 400
       ENDIF
 401   CONTINUE
C
       CALL MY_BARRIER
C
       DO 440 I=NISKYFT,NISKYLT
         ISKY(I) = 0
 440   CONTINUE
       NISKY = 0
C adsKy est decale de 1 
C-----------------------------------------------
C     FORCES D'INTERFACES
C-----------------------------------------------
       IF (IVECTOR==0) THEN
         IVSIZE = 1024
       ELSE
         IVSIZE = VSIZE
       ENDIF
 600   CONTINUE
       CALL IDP_LOCK(6)
       IF(INTDONE>NUMNOD)THEN
        CALL IDP_FREE(6)
        GO TO 601
       ELSE
        NDFT=INTDONE
        INTDONE=INTDONE+IVSIZE
        CALL IDP_FREE(6)
       ENDIF
       NDLT=MIN(NDFT+IVSIZE-1,NUMNOD)

       K1 = 0
       K2 = 0
       K3 = 0
       DO N=NDFT,NDLT
         NN = N-1
         DIFFADD = ADSKYI(N)-1-ADSKYI(NN)
         IF(DIFFADD==0) THEN
           VIND1(NDFT+K1) = N
           K1 = K1 + 1
Cel 20 valeur seuil pour appel a qsort (20 = NBCC)
         ELSEIF(DIFFADD>=1.AND.DIFFADD<NBCC) THEN
           VIND2(NDFT+K2) = N
           K2 = K2 + 1
         ELSEIF(DIFFADD>=NBCC) THEN
           VIND3(NDFT+K3) = N
           K3 = K3 + 1
         ENDIF
       ENDDO
C
C une seule contribution
C
       NEWN = K1
       IF(INTHEAT == 0 ) THEN
         IF(KDTINT==0)THEN
#include "vectorize.inc"
          DO IJK=NDFT,NDFT+NEWN-1
           N = VIND1(IJK)
           K=ADSKYI(N-1)
           A(1,N)  = A(1,N)  + FSKYI(K,1)
           A(2,N)  = A(2,N)  + FSKYI(K,2)
           A(3,N)  = A(3,N)  + FSKYI(K,3)
           STIFN(N) = STIFN(N) + FSKYI(K,4)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO IJK=NDFT,NDFT+NEWN-1
           N = VIND1(IJK)
           K=ADSKYI(N-1)
           A(1,N)  = A(1,N)  + FSKYI(K,1)
           A(2,N)  = A(2,N)  + FSKYI(K,2)
           A(3,N)  = A(3,N)  + FSKYI(K,3)
           STIFN(N) = STIFN(N) + FSKYI(K,4)
           VISCN(N) = VISCN(N) + FSKYI(K,5)
          ENDDO
         ENDIF
C  + la thermique 
        ELSE
         IF(KDTINT==0)THEN
#include "vectorize.inc"
          DO IJK=NDFT,NDFT+NEWN-1
           N = VIND1(IJK)
           K=ADSKYI(N-1)
           A(1,N)  = A(1,N)  + FSKYI(K,1)
           A(2,N)  = A(2,N)  + FSKYI(K,2)
           A(3,N)  = A(3,N)  + FSKYI(K,3)
           STIFN(N) = STIFN(N) + FSKYI(K,4)
           FTHE(N)  = FTHE(N)  + FTHESKYI(K)
          ENDDO
         ELSE
#include "vectorize.inc"
          DO IJK=NDFT,NDFT+NEWN-1
           N = VIND1(IJK)
           K=ADSKYI(N-1)
           A(1,N)  = A(1,N)  + FSKYI(K,1)
           A(2,N)  = A(2,N)  + FSKYI(K,2)
           A(3,N)  = A(3,N)  + FSKYI(K,3)
           STIFN(N) = STIFN(N) + FSKYI(K,4)
           VISCN(N) = VISCN(N) + FSKYI(K,5)
           FTHE(N)  = FTHE(N)  + FTHESKYI(K)
          ENDDO
         ENDIF
       ENDIF
C contact stiffness (static condensation)
       IF(ISTATCND/=0)THEN
#include "vectorize.inc"
          DO IJK=NDFT,NDFT+NEWN-1
           N = VIND1(IJK)
           K=ADSKYI(N-1)
           STCND(N) = STCND(N) + FSKYI(K,4)
          END DO
       ENDIF
C
C pas plus de 20 contributions
C
       NEWN = K2
       IF (IVECTOR==0) THEN
        DO 800 IJK=NDFT,NDFT+NEWN-1
         N = VIND2(IJK)
         NN = N-1
         JJ1 = ADSKYI(NN)
         JJ2 = ADSKYI(N)-1
C-----------------------------------------------
C       TRI DES FORCES D'INTERFACES
C-----------------------------------------------
        IF(INTHEAT == 0 ) THEN
         DO 500 K=JJ1,JJ2-1
           DO 500 KK=K+1,JJ2
             DO 500 L=1,NFSKYI
               IF(FSKYI(KK,L)>FSKYI(K,L))THEN
                 FF = FSKYI(KK,L)
                 FSKYI(KK,L) = FSKYI(K,L)
                 FSKYI(K,L)  = FF
               ENDIF
 500       CONTINUE
         ELSE
          DO  K=JJ1,JJ2-1
           DO  KK=K+1,JJ2
             DO  L=1,NFSKYI
               IF(FSKYI(KK,L)>FSKYI(K,L))THEN
                 FF = FSKYI(KK,L)
                 FSKYI(KK,L) = FSKYI(K,L)
                 FSKYI(K,L)  = FF
               ENDIF
             ENDDO
C             
             IF(FTHESKYI(KK) > FTHESKYI(K))THEN
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
         IF(KDTINT==0)THEN
          DO K=JJ1,JJ2
           A(1,N)  = A(1,N)  + FSKYI(K,1)
           A(2,N)  = A(2,N)  + FSKYI(K,2)
           A(3,N)  = A(3,N)  + FSKYI(K,3)
           STIFN(N) = STIFN(N) + FSKYI(K,4)
          ENDDO
         ELSE
          DO K=JJ1,JJ2
           A(1,N)  = A(1,N)  + FSKYI(K,1)
           A(2,N)  = A(2,N)  + FSKYI(K,2)
           A(3,N)  = A(3,N)  + FSKYI(K,3)
           STIFN(N) = STIFN(N) + FSKYI(K,4)
           VISCN(N) = VISCN(N) + FSKYI(K,5)
          ENDDO
         ENDIF
C  + la thermique 
         ELSE
          IF(KDTINT==0)THEN
           DO K=JJ1,JJ2
            A(1,N)  = A(1,N)  + FSKYI(K,1)
            A(2,N)  = A(2,N)  + FSKYI(K,2)
            A(3,N)  = A(3,N)  + FSKYI(K,3)
            STIFN(N) = STIFN(N) + FSKYI(K,4)
            FTHE(N)  = FTHE(N)  + FTHESKYI(K)
           ENDDO
          ELSE
           DO K=JJ1,JJ2
            A(1,N)  = A(1,N)  + FSKYI(K,1)
            A(2,N)  = A(2,N)  + FSKYI(K,2)
            A(3,N)  = A(3,N)  + FSKYI(K,3)
            STIFN(N) = STIFN(N) + FSKYI(K,4)
            VISCN(N) = VISCN(N) + FSKYI(K,5)
            FTHE(N)  = FTHE(N)  + FTHESKYI(K)
           ENDDO
          ENDIF
         ENDIF
C contact stiffness (static condensation)
         IF(ISTATCND/=0)THEN
             DO K=JJ1,JJ2
             STCND(N) = STCND(N) + FSKYI(K,4)
            END DO
         ENDIF
 800    CONTINUE
       ELSE
C --- START-UP 
C     DECIDE WHICH TREATMENT SHALL BE USED
C       USE OLD TREATMENT FOR NEWN < 100
C       USE NEW TREATMENT AND DECIDE WHERE TO USE NEW LOOP
C           AND WHERE TO STAY WITH OLD LOOP 
         IF (NEWN<100) THEN
           KSPLIT=1
           DO I=1,NEWN
              IARRAY(I)=I
           ENDDO
           DO I=1,NBCC+1
              IC(I)=1
           ENDDO
         ELSE
           KMAX=0
#include "vectorize.inc"
           DO I=1,NEWN
              N=VIND2(NDFT+I-1)
              JJ1=ADSKYI(N-1)
              JJ2=ADSKYI(N)-1
              LJ=JJ2-JJ1+1
              IF(LJ>KMAX) KMAX=LJ
              NN_A(I)=LJ
           ENDDO

           CALL ORDER_V(NEWN,1,KMAX,NN_A,IARRAY,IC)
           IC(KMAX+1)=IC(KMAX)+1
C
           KSPLIT=1
           DO I=2,KMAX
              IF(NEWN-IC(I)>NBCC) KSPLIT=I
           ENDDO
        ENDIF

C --- ORDERING LOOP
        KKEND=IC(KSPLIT+1)-1
        DO K=1,KSPLIT-1
           DO KK=K+1,KSPLIT
              KKSTART=IC(KK)
#include "vectorize.inc"
              DO I=KKSTART,KKEND
                 IJK=NDFT+IARRAY(I)-1
                 N=VIND2(IJK)
                 J=ADSKYI(N-1)+K-1
                 JJ=ADSKYI(N-1)+KK-1
C L=1
                 IF (FSKYI(JJ,1)>FSKYI(J,1)) THEN
                    FF=FSKYI(JJ,1)
                    FSKYI(JJ,1)=FSKYI(J,1)
                    FSKYI(J,1) =FF
                 ENDIF
C L=2
                 IF (FSKYI(JJ,2)>FSKYI(J,2)) THEN
                    FF=FSKYI(JJ,2)
                    FSKYI(JJ,2)=FSKYI(J,2)
                    FSKYI(J,2) =FF
                 ENDIF
C L=3
                 IF (FSKYI(JJ,3)>FSKYI(J,3)) THEN
                    FF=FSKYI(JJ,3)
                    FSKYI(JJ,3)=FSKYI(J,3)
                    FSKYI(J,3) =FF
                 ENDIF
C L=4
                 IF (FSKYI(JJ,4)>FSKYI(J,4)) THEN
                    FF=FSKYI(JJ,4)
                    FSKYI(JJ,4)=FSKYI(J,4)
                    FSKYI(J,4) =FF
                 ENDIF
C
                 IF(INTHEAT > 0) THEN
                  IF (FTHESKYI(JJ)>FTHESKYI(J)) THEN
                    FF=FTHESKYI(JJ)
                    FTHESKYI(JJ)=FTHESKYI(J)
                    FTHESKYI(J) =FF
                  ENDIF
                 ENDIF
C                 
              ENDDO
           ENDDO
        ENDDO
C L=5
        IF(KDTINT/=0)THEN
         KKEND=IC(KSPLIT+1)-1
         DO K=1,KSPLIT-1
           DO KK=K+1,KSPLIT
              KKSTART=IC(KK)
#include "vectorize.inc"
              DO I=KKSTART,KKEND
                 IJK=NDFT+IARRAY(I)-1
                 N=VIND2(IJK)
                 J=ADSKYI(N-1)+K-1
                 JJ=ADSKYI(N-1)+KK-1
                 IF (FSKYI(JJ,5)>FSKYI(J,5)) THEN
                    FF=FSKYI(JJ,5)
                    FSKYI(JJ,5)=FSKYI(J,5)
                    FSKYI(J,5) =FF
                 ENDIF
              ENDDO
           ENDDO
         ENDDO
        ENDIF
C --- FILL ARRAY A
       IF(INTHEAT == 0 ) THEN
        IF(KDTINT==0)THEN
         DO K=1,KSPLIT
           KKSTART=IC(K)
#include "vectorize.inc"
           DO I=KKSTART,KKEND
              IJK=NDFT+IARRAY(I)-1
              N=VIND2(IJK)
              J=ADSKYI(N-1)+K-1
              A(1,N) = A(1,N) + FSKYI(J,1)
              A(2,N) = A(2,N) + FSKYI(J,2)
              A(3,N) = A(3,N) + FSKYI(J,3)
              STIFN(N) = STIFN(N) + FSKYI(J,4)
           ENDDO
         ENDDO
        ELSE
         DO K=1,KSPLIT
           KKSTART=IC(K)
#include "vectorize.inc"
           DO I=KKSTART,KKEND
              IJK=NDFT+IARRAY(I)-1
              N=VIND2(IJK)
              J=ADSKYI(N-1)+K-1
              A(1,N) = A(1,N) + FSKYI(J,1)
              A(2,N) = A(2,N) + FSKYI(J,2)
              A(3,N) = A(3,N) + FSKYI(J,3)
              STIFN(N) = STIFN(N) + FSKYI(J,4)
              VISCN(N) = VISCN(N) + FSKYI(J,5)
           ENDDO
         ENDDO
        ENDIF
C + la thermique 
       ELSE
        IF(KDTINT==0)THEN
         DO K=1,KSPLIT
           KKSTART=IC(K)
#include "vectorize.inc"
           DO I=KKSTART,KKEND
              IJK=NDFT+IARRAY(I)-1
              N=VIND2(IJK)
              J=ADSKYI(N-1)+K-1
              A(1,N) = A(1,N) + FSKYI(J,1)
              A(2,N) = A(2,N) + FSKYI(J,2)
              A(3,N) = A(3,N) + FSKYI(J,3)
              STIFN(N) = STIFN(N) + FSKYI(J,4)
              FTHE(N) = FTHE(N) + FTHESKYI(J)
           ENDDO
         ENDDO
        ELSE
         DO K=1,KSPLIT
           KKSTART=IC(K)
#include "vectorize.inc"
           DO I=KKSTART,KKEND
              IJK=NDFT+IARRAY(I)-1
              N=VIND2(IJK)
              J=ADSKYI(N-1)+K-1
              A(1,N) = A(1,N) + FSKYI(J,1)
              A(2,N) = A(2,N) + FSKYI(J,2)
              A(3,N) = A(3,N) + FSKYI(J,3)
              STIFN(N) = STIFN(N) + FSKYI(J,4)
              VISCN(N) = VISCN(N) + FSKYI(J,5)
              FTHE(N) = FTHE(N) + FTHESKYI(J)
           ENDDO
         ENDDO
        ENDIF
       ENDIF
C contact stiffness (static condensation)
       IF(ISTATCND/=0)THEN
         KKSTART=IC(K)
#include "vectorize.inc"
         DO I=KKSTART,KKEND
            IJK=NDFT+IARRAY(I)-1
            N=VIND2(IJK)
            J=ADSKYI(N-1)+K-1
            STCND(N) = STCND(N) + FSKYI(J,4)
         END DO
       ENDIF
C --- DO THE REST IN OLD WAY - SUM UP ALL CONTRIBUTIONS PER NODE
        NSTART=IC(KSPLIT+1)
        DO 801 I=NSTART,NEWN
           IJK=NDFT+IARRAY(I)-1
           N=VIND2(IJK)
           NN=N-1
           JJ1=ADSKYI(NN)
           JJ2=ADSKYI(N)-1
           IF(INTHEAT == 0 ) THEN
            DO 501 K=JJ1,JJ2-1
              DO 501 KK=K+1,JJ2
               DO 501 L=1,NFSKYI
                 IF(FSKYI(KK,L)>FSKYI(K,L)) THEN
                   FF=FSKYI(KK,L)
                   FSKYI(KK,L)=FSKYI(K,L)
                   FSKYI(K,L)=FF
                 ENDIF
  501          CONTINUE
           ELSE
            DO  K=JJ1,JJ2-1
              DO  KK=K+1,JJ2
               DO  L=1,NFSKYI
                 IF(FSKYI(KK,L)>FSKYI(K,L)) THEN
                   FF=FSKYI(KK,L)
                   FSKYI(KK,L)=FSKYI(K,L)
                   FSKYI(K,L)=FF
                 ENDIF
               ENDDO
               IF(FTHESKYI(KK)>FTHESKYI(K)) THEN
                   FF=FTHESKYI(KK)
                   FTHESKYI(KK)=FTHESKYI(K)
                   FTHESKYI(K)=FF
               ENDIF
              ENDDO
             ENDDO
           ENDIF
C
        IF(INTHEAT == 0 )THEN
          IF(KDTINT==0)THEN
#include "scalar.inc"
             DO K=JJ1,JJ2
                A(1,N) = A(1,N) + FSKYI(K,1)
                A(2,N) = A(2,N) + FSKYI(K,2)
                A(3,N) = A(3,N) + FSKYI(K,3)
                STIFN(N) = STIFN(N) + FSKYI(K,4)
             ENDDO
          ELSE
#include "scalar.inc"
             DO K=JJ1,JJ2
                A(1,N) = A(1,N) + FSKYI(K,1)
                A(2,N) = A(2,N) + FSKYI(K,2)
                A(3,N) = A(3,N) + FSKYI(K,3)
                STIFN(N) = STIFN(N) + FSKYI(K,4)
                VISCN(N) = VISCN(N) + FSKYI(K,5)
             ENDDO
          ENDIF
         ELSE
          IF(KDTINT==0)THEN
#include "scalar.inc"
             DO K=JJ1,JJ2
                A(1,N) = A(1,N) + FSKYI(K,1)
                A(2,N) = A(2,N) + FSKYI(K,2)
                A(3,N) = A(3,N) + FSKYI(K,3)
                STIFN(N) = STIFN(N) + FSKYI(K,4)
                FTHE(N) = FTHE(N) + FTHESKYI(K)
             ENDDO
          ELSE
#include "scalar.inc"
             DO K=JJ1,JJ2
                A(1,N) = A(1,N) + FSKYI(K,1)
                A(2,N) = A(2,N) + FSKYI(K,2)
                A(3,N) = A(3,N) + FSKYI(K,3)
                STIFN(N) = STIFN(N) + FSKYI(K,4)
                VISCN(N) = VISCN(N) + FSKYI(K,5)
                FTHE(N) = FTHE(N) + FTHESKYI(K)
             ENDDO
          ENDIF
        ENDIF
C contact stiffness (static condensation)
        IF(ISTATCND/=0)THEN
#include "scalar.inc"
          DO K=JJ1,JJ2
             STCND(N) = STCND(N) + FSKYI(K,4)
          END DO
        ENDIF

  801   CONTINUE
       ENDIF
C FIN NOUVEAUX TRAITEMENTS VECTORIELS
C
C plus de 20 contributions
C
       NEWN = K3
       DO IJK=NDFT,NDFT+NEWN-1
         N = VIND3(IJK)
         NN = N-1
         JJ1 = ADSKYI(NN)
         JJ2 = ADSKYI(N)-1
C-----------------------------------------------
C       TRI DES FORCES D'INTERFACES (QSORT)
C-----------------------------------------------
         CALL ASS2SORT(FSKYI,JJ1,JJ2,FSKYT,NFSKYI)
         IF(INTHEAT >  0) CALL ASS2SORT(FTHESKYI,JJ1,JJ2,FSKYT,NFSKYI)
C-----------------------------------------------
C       ASSEMBLAGE DES FORCES 
C-----------------------------------------------
        IF(INTHEAT == 0 ) THEN
          IF(KDTINT==0)THEN
           DO K=JJ1,JJ2
             A(1,N)  = A(1,N)  + FSKYI(K,1)
             A(2,N)  = A(2,N)  + FSKYI(K,2)
             A(3,N)  = A(3,N)  + FSKYI(K,3)
             STIFN(N) = STIFN(N) + FSKYI(K,4)
           ENDDO
          ELSE
           DO K=JJ1,JJ2
             A(1,N)  = A(1,N)  + FSKYI(K,1)
             A(2,N)  = A(2,N)  + FSKYI(K,2)
             A(3,N)  = A(3,N)  + FSKYI(K,3)
             STIFN(N) = STIFN(N) + FSKYI(K,4)
             VISCN(N) = VISCN(N) + FSKYI(K,5)
           ENDDO
          ENDIF
C + la thermique
        ELSE
          IF(KDTINT==0)THEN
           DO K=JJ1,JJ2
             A(1,N)  = A(1,N)  + FSKYI(K,1)
             A(2,N)  = A(2,N)  + FSKYI(K,2)
             A(3,N)  = A(3,N)  + FSKYI(K,3)
             STIFN(N) = STIFN(N) + FSKYI(K,4)
             FTHE(N) = FTHE(N) + FTHESKYI(K)
           ENDDO
          ELSE
           DO K=JJ1,JJ2
             A(1,N)  = A(1,N)  + FSKYI(K,1)
             A(2,N)  = A(2,N)  + FSKYI(K,2)
             A(3,N)  = A(3,N)  + FSKYI(K,3)
             STIFN(N) = STIFN(N) + FSKYI(K,4)
             VISCN(N) = VISCN(N) + FSKYI(K,5)
             FTHE(N) = FTHE(N) + FTHESKYI(K)
           ENDDO
          ENDIF
        ENDIF
C contact stiffness (static condensation)
        IF(ISTATCND/=0)THEN
          DO K=JJ1,JJ2
             STCND(N) = STCND(N) + FSKYI(K,4)
          END DO
        ENDIF
       ENDDO
       GOTO 600
 601   CONTINUE
C fin nisky/=0
      ENDIF
C-----------------------------------------------
C
C  003      NUM7 = 7*NPART
      NUM7 = NPSAV*NPART
C
      KM = 0
Celdev036#include "vectorize.inc"
      DO 950 K=1,NTHREAD-1
          KM = KM + NUM7
Celdev036+1
#include "vectorize.inc"
          DO 940 I=PARTFT,PARTLT
           PARTSAV(I) = PARTSAV(I) + PARTSAV(I+KM)
           PARTSAV(I+KM) = ZERO
 940      CONTINUE
 950  CONTINUE
C
      CALL MY_BARRIER
C
      RETURN
      END
C
Cel routines de vectorisation de asspar2
Chd|====================================================================
Chd|  ORDER_V                       source/assembly/asspar2.F     
Chd|-- called by -----------
Chd|        ASSPAR2                       source/assembly/asspar2.F     
Chd|        ASSPAR4                       source/assembly/asspar4.F     
Chd|-- calls ---------------
Chd|        REALLY_ORDER_V                source/assembly/asspar2.F     
Chd|        SHORT_ORDER_V                 source/assembly/asspar2.F     
Chd|====================================================================
      SUBROUTINE ORDER_V(N, MINNN_GLB, MAXNN_GLB, NN_A, IARRAY, IC)
c------------------------------------------------------------------
c        NEC European HPC Technology Centre
c        Hessbruehlstr. 21B
c        70565 Stuttgart                     phone: +49-711-78055-12
c        Germany                             fax  :       "      -25
c
c        Author: M. Galle                        Q4 2000
c
c        DO NOT REMOVE THIS HEADER
c------------------------------------------------------------------
c-nec In this routine, the array iarray is filled with numbers
c-nec according to the entry in the array nn_a. In the array ic
c-nec the address of the first entry for a new nn is stored.
c-nec minnn and maxnn are the boundaries of the range of nn.

c-nec -------------------------------------------------------------
c-nec IN:
c-nec ===
c-nec n:         number of elements to be ordered
c-nec minnn_glb: smallest value in the array nn_a
c-nec maxnn_glb: largest value in the array nn_a
c-nec nn_a:      integer array containing the information the

c-nec            ordering is based upon
c-nec OUT:
c-nec ====
c-nec ic:        addres for the first entry in iarray for each new
c-nec            value
c-nec iarray:    ordered array of the n elements
c-nec -------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, MINNN_GLB, MAXNN_GLB, IARRAY(*), NN_A(*), IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MINNN, MAXNN, NN, I, IC0
      INTEGER DIFFNN, DIVNN, DELNN
C-----------------------------------------------
      IF(MAXNN_GLB<MINNN_GLB .OR. N==0) RETURN
C
      IF(N == 1) THEN
        DO NN = MINNN_GLB, MAXNN_GLB
          IC(NN) = 1
        END DO
        IARRAY(1) = 1
        RETURN
      ELSE IF(MAXNN_GLB == MINNN_GLB) THEN
        IC(MAXNN_GLB) = 1
        DO I = 1, N
          IARRAY(I) = I
        END DO
        RETURN
      END IF
C
      DIFFNN = MAXNN_GLB - MINNN_GLB
      IF(DIFFNN <= 4) THEN
        CALL SHORT_ORDER_V(N, MINNN_GLB, MAXNN_GLB, NN_A, IARRAY, IC)
      ELSE
        IF(DIFFNN > 2**16) THEN
          DIVNN = 16
        ELSE IF(DIFFNN > 2**15) THEN
          DIVNN = 8
        ELSE IF(DIFFNN > 2**14) THEN
          DIVNN = 4
        ELSE IF(DIFFNN > 2**13) THEN
          DIVNN = 2
        ELSE
          DIVNN = 1
        END IF
        DELNN = (DIFFNN + DIVNN) / DIVNN
        IC0 = 1
        DO MINNN = MINNN_GLB, MAXNN_GLB - 1, DELNN
          MAXNN = MIN(MAXNN_GLB, MINNN + DELNN - 1)
          CALL REALLY_ORDER_V(N, MINNN, MAXNN, NN_A,
     &                        IARRAY, IC, IC0)
        END DO
      END IF
C
      RETURN
      END
Chd|====================================================================
Chd|  SHORT_ORDER_V                 source/assembly/asspar2.F     
Chd|-- called by -----------
Chd|        ORDER_V                       source/assembly/asspar2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHORT_ORDER_V(N, MINNN, MAXNN, NN_A, IARRAY, IC)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, MINNN, MAXNN, IARRAY(*), NN_A(*), IC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, NN, COUNT
C-----------------------------------------------
      COUNT = 0
C
      DO NN = MINNN, MAXNN
        IC(NN) = COUNT + 1
        DO I = 1, N
          IF(NN_A(I) == NN) THEN
            COUNT = COUNT + 1
            IARRAY(COUNT) = I
          END IF
        END DO
      END DO
C
      RETURN
      END
Chd|====================================================================
Chd|  REALLY_ORDER_V                source/assembly/asspar2.F     
Chd|-- called by -----------
Chd|        ORDER_V                       source/assembly/asspar2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE REALLY_ORDER_V(n, minnn, maxnn, nn_a, iarray, ic, ic0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------

C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER n, minnn, maxnn, iarray(*), nn_a(*), ic(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NBCC
      PARAMETER (NBCC = 20)
      INTEGER NN, IC0, IN, IV, I, J, IC_V(256,NBCC+1)
C-----------------------------------------------
C---  INITIALIZE IC_V
      DO J = MINNN, MAXNN
        DO I = 1, 256
           ic_v(i,j) = 0
        ENDDO
      ENDDO
C
C---  DETERMINE THE NUMBER OF OCCURANCES OF NN FOR EACH VECTOR
CCCCCCCCCCCCCCCCCCCC!cdir nodep
      DO I = 1, N
        IV = MOD(I-1,256) + 1
        NN = NN_A(I)
        IF(NN>=MINNN .AND. NN<=MAXNN)
     &       IC_V(IV,NN) = IC_V(IV,NN) + 1
      END DO

C---  REDUCE IC_V(1-256,IN) TO IC(IN)
      IC(MINNN) = IC0
      DO IN = MINNN+1, MAXNN
        IC(IN) = IC(IN-1)
        DO IV = 1, 256
          IC(IN) = IC(IN) + IC_V(IV,IN-1)
        END DO
      END DO

C---  SET IC_V FOR FIRST VECTOR
      DO IN = MINNN, MAXNN
        IC_V(1,IN) = IC(IN) + IC_V(1,IN)
      END DO

C---  SET IC_V FOR ALL OTHER VECTORS
      DO IN = MINNN, MAXNN
CCCCCCCCCCCCCC!cdir unroll=255
        DO IV = 2, 256
          IC_V(IV,IN) = IC_V(IV-1,IN) + IC_V(IV,IN)
        END DO
      END DO

C---  CORRECT IC_V FOR THE VECTORS 256...2
      DO IN = MINNN, MAXNN
CCCCCCCCCCCCCC!cdir unroll=255
        DO IV = 256, 2, -1
          IC_V(IV,IN) = IC_V(IV-1,IN)
        END DO
      END DO

C---  CORRECT IC_V FOR VECTOR 1
      DO IN = MINNN, MAXNN
        IC_V(1,IN) = IC(IN)
      END DO

C---  NOW FILL IARRAY
CCCCCCCCCCCCCCC!cdir nodep
      DO I = 1, N
        IV = MOD(I-1,256) + 1
        NN = NN_A(I)
        IF(NN>=MINNN .AND. NN<=MAXNN) THEN
          IARRAY(IC_V(IV,NN)) = I
          IC_V(IV,NN) = IC_V(IV,NN) + 1
        END IF
      END DO
C
      IC0 = IC_V(256,MAXNN)
C
      RETURN
      END
