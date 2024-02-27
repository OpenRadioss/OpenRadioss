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
Chd|  ASSPAR4                       source/assembly/asspar4.F     
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        ASS2SORT                      source/assembly/ass2sort.F    
Chd|        ASS2SORT_PXFEM                source/assembly/ass2sort.F    
Chd|        MY_BARRIER                    source/system/machine.F       
Chd|        ORDER_V                       source/assembly/asspar2.F     
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE ASSPAR4(
     1  A       ,AR      ,STIFN  ,STIFR  ,MS      ,
     2  FSKY    ,FSKYV   ,ADDCNE ,INDSKY ,FSKYM   ,
     3  MSNF    ,ISKY    ,FSKYI  ,VISCN  ,FTHE    ,
     4  FTHESKY ,FTHESKYI,NODFT  ,NODLT  ,ADSKYI  ,
     5  PARTSAV ,PARTFT  ,PARTLT ,ITASK  ,GREFT   ,
     6  GRELT   ,GRESAV  ,AF     ,FFSKY  ,MSF     ,
     7  ADDCNE_PXFEM ,INOD_PXFEM ,ITAB   ,FSKYD   ,
     8  DMSPH   ,CONDN,CONDNSKY,CONDNSKYI,MS_2D,ICNDS10 ,
     A  STIFND  ,FORNEQS ,FORNEQSKY,NFACNIT,NODFT_2,
     B  NODLT_2 ,FSKY_L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
      USE ALE_MOD
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
#include      "task_c.inc"
#include      "scr18_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
      INTEGER MAXBLOC,NBLOC,NBVAL,NBCOL,NFACNIT
      PARAMETER (MAXBLOC=1000)
      COMMON/PTMPARIT/NBLOC,NBVAL(1:MAXBLOC),NBCOL(1:MAXBLOC)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C----------------------------------------------
      INTEGER ADDCNE(*),INDSKY(*), ISKY(*),
     .        ADSKYI(0:NUMNOD+1),
     .        NODFT, NODLT, ITASK, PARTFT, PARTLT,GREFT,GRELT,
     .        ADDCNE_PXFEM(*),INOD_PXFEM(*), ITAB(*),ICNDS10(3,*),NODFT_2  ,NODLT_2

C     REAL
      my_real
     .   A(3,*)  ,AR(3,*),STIFN(*),STIFR(*),FSKYV(LSKY,8),FSKY(8,LSKY),
     .   MS(*), FSKYM(*), MSNF(*), VISCN(*), FSKYI(LSKYI,NFSKYI),
     .   FTHE(*), FTHESKY(LSKY), FTHESKYI(LSKYI), PARTSAV(*),GRESAV(*),
     .   AF(3,*),FFSKY(3,*),MSF(*), FSKYD(*), DMSPH(*),CONDNSKY(LSKY),CONDN(*),
     .   CONDNSKYI(LSKYI),MS_2D(*),STIFND(*),FORNEQS(3,*) ,
     .   FORNEQSKY(3*NFACNIT,*)
      my_real, DIMENSION(NISKY), INTENT(INOUT) :: FSKY_L !< working array for FSKY sorting
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER VSIZE, NBCC, NUM7, KM,IL,IPLY,ND
      PARAMETER (VSIZE = 8192)
      PARAMETER (NBCC = 20)
      INTEGER I,J,K,L,N,NC,IDEB,LB,KK,J1,J2,JJ1,JJ2,NN,
     .        NL,IFT,ILT,KFT,KLT,IVSIZE,
     .        NISKYFT,NISKYLT,NFSKYFT,NFSKYLT,
     .        K1,K2,K3,K4,K5,K6,K7,K8,K9,K0,I0,IJK,NF,
     .        NCT,DIFFADD,NDLT,NDFT,NI,LJ,KKEND,JJ,NSTART,
     .        KKSTART,KMAX,KSPLIT,IC(NBCC+1),NN_A(VSIZE),IARRAY(VSIZE),
     .        NFSKYFT_INTPLY,NFSKYLT_INTPLY,
     .        ITAG, ITAG1,ICHECK,ICHECK1
      my_real
     .        FF, FSKYT(NISKY)
      INTEGER :: VIND_SIZE
      INTEGER, DIMENSION(:), ALLOCATABLE :: VIND1,VIND2,VIND3
      INTEGER :: CHUNK_NODE,CHUNK_ISKY,CHUNK_NODE_1
C-----------------------------------------------
C Partie ELEMENT
C-----------------------------------------------
     
      IVSIZE = 32

      VIND_SIZE=MAX(NODLT-NODFT+1,IVSIZE)
      ALLOCATE(VIND1(VIND_SIZE))
      ALLOCATE(VIND2(VIND_SIZE))
      ALLOCATE(VIND3(VIND_SIZE))
      CHUNK_NODE = int(NUMNOD / (10*NTHREAD))
      if( CHUNK_NODE<2) CHUNK_NODE = int(NUMNOD/NTHREAD)
      CHUNK_NODE = max(1,CHUNK_NODE)
      CHUNK_NODE_1 = (NUMNOD+2)/ (10*NTHREAD)
      if( CHUNK_NODE_1<2) CHUNK_NODE_1 = (NUMNOD+1)/NTHREAD
      CHUNK_NODE_1 = max(1,CHUNK_NODE_1)

!$OMP DO SCHEDULE(dynamic,CHUNK_NODE_1) 
      DO N = 1,NUMNOD+1
        ADSKYI(N) = 0
      ENDDO
!$OMP END DO

!$OMP SINGLE
      DO I=1,NISKY
        N = ISKY(I)+1
        ADSKYI(N) = ADSKYI(N)+1
      ENDDO
C-----------------------------------------------
C   CALCUL DES ADRESSES DU VECTEUR SKYLINE
C-----------------------------------------------
      ADSKYI(0) = 1
      ADSKYI(1) = 1
      DO N = 1, NUMNOD
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
!$OMP END SINGLE NOWAIT

      ! ------------------------------      
      IF(ITHERM_FE == 0 ) THEN
        ! --------------
        IF(ALE%SUB%IFSUBM==0)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
          DO N = 1,NUMNOD 
            NCT = ADDCNE(N)-1
            NC = ADDCNE(N+1)-ADDCNE(N)
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
!$OMP END DO
        ! --------------
        ELSEIF(N2D/=0)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
          DO N = 1,NUMNOD
            NCT = ADDCNE(N)-1
            NC = ADDCNE(N+1)-ADDCNE(N)
            DO K = NCT+1, NCT+NC
              A(1,N)   = A(1,N)   + FSKY(1,K)
              A(2,N)   = A(2,N)   + FSKY(2,K)
              A(3,N)   = A(3,N)   + FSKY(3,K)
              AR(1,N)  = AR(1,N)  + FSKY(4,K)
              AR(2,N)  = AR(2,N)  + FSKY(5,K)
              AR(3,N)  = AR(3,N)  + FSKY(6,K)
              STIFN(N) = STIFN(N) + FSKY(7,K)
              STIFR(N) = STIFR(N) + FSKY(8,K)
              MS_2D(N) = MS_2D(N)  + FSKYM(K)
            ENDDO
          ENDDO
!$OMP END DO
        ! --------------
        ELSE
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
          DO N = 1,NUMNOD
            NCT = ADDCNE(N)-1
            NC = ADDCNE(N+1)-ADDCNE(N)
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
!$OMP END DO
        ENDIF
        ! --------------
      ! ------------------------------ 
      ELSE
        ! --------------
        IF(NODADT_THERM == 1) THEN
          ! --------------
          IF(ALE%SUB%IFSUBM==0)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
            DO N = 1,NUMNOD
              NCT = ADDCNE(N)-1
              NC = ADDCNE(N+1)-ADDCNE(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKY(1,K)
                A(2,N)   = A(2,N)   + FSKY(2,K)
                A(3,N)   = A(3,N)   + FSKY(3,K)
                AR(1,N)  = AR(1,N)  + FSKY(4,K)
                AR(2,N)  = AR(2,N)  + FSKY(5,K)
                AR(3,N)  = AR(3,N)  + FSKY(6,K)
                STIFN(N) = STIFN(N) + FSKY(7,K)
                STIFR(N) = STIFR(N) + FSKY(8,K)
                FTHE(N)  = FTHE(N)  + FTHESKY(K)
                CONDN(N) = CONDN(N) + CONDNSKY(K)
              ENDDO
            ENDDO
!$OMP END DO
          ! --------------
          ELSEIF(N2D/=0)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
            DO N = 1,NUMNOD
              NCT = ADDCNE(N)-1
              NC = ADDCNE(N+1)-ADDCNE(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKY(1,K)
                A(2,N)   = A(2,N)   + FSKY(2,K)
                A(3,N)   = A(3,N)   + FSKY(3,K)
                AR(1,N)  = AR(1,N)  + FSKY(4,K)
                AR(2,N)  = AR(2,N)  + FSKY(5,K)
                AR(3,N)  = AR(3,N)  + FSKY(6,K)
                STIFN(N) = STIFN(N) + FSKY(7,K)
                STIFR(N) = STIFR(N) + FSKY(8,K)
                FTHE(N)  = FTHE(N)  + FTHESKY(K)
                CONDN(N) = CONDN(N) + CONDNSKY(K)
                MS_2D(N) = MS_2D(N)  + FSKYM(K)
              ENDDO
            ENDDO
!$OMP END DO
          ! --------------
          ELSE
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
            DO N = 1,NUMNOD
              NCT = ADDCNE(N)-1
              NC = ADDCNE(N+1)-ADDCNE(N)
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
                FTHE(N)  = FTHE(N)  + FTHESKY(K)
                CONDN(N) = CONDN(N) + CONDNSKY(K)
              ENDDO
            ENDDO
!$OMP END DO
          ENDIF
          ! --------------
        ! --------------
        ELSE ! NODADT_THERM/=1
          ! --------------
          IF(ALE%SUB%IFSUBM==0)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
            DO N = 1,NUMNOD
              NCT = ADDCNE(N)-1
              NC = ADDCNE(N+1)-ADDCNE(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKY(1,K)
                A(2,N)   = A(2,N)   + FSKY(2,K)
                A(3,N)   = A(3,N)   + FSKY(3,K)
                AR(1,N)  = AR(1,N)  + FSKY(4,K)
                AR(2,N)  = AR(2,N)  + FSKY(5,K)
                AR(3,N)  = AR(3,N)  + FSKY(6,K)
                STIFN(N) = STIFN(N) + FSKY(7,K)
                STIFR(N) = STIFR(N) + FSKY(8,K)
                FTHE(N)  = FTHE(N)  + FTHESKY(K)
              ENDDO
            ENDDO
!$OMP END DO
          ! --------------
          ELSEIF(N2D/=0)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
            DO N = 1,NUMNOD
              NCT = ADDCNE(N)-1
              NC = ADDCNE(N+1)-ADDCNE(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKY(1,K)
                A(2,N)   = A(2,N)   + FSKY(2,K)
                A(3,N)   = A(3,N)   + FSKY(3,K)
                AR(1,N)  = AR(1,N)  + FSKY(4,K)
                AR(2,N)  = AR(2,N)  + FSKY(5,K)
                AR(3,N)  = AR(3,N)  + FSKY(6,K)
                STIFN(N) = STIFN(N) + FSKY(7,K)
                STIFR(N) = STIFR(N) + FSKY(8,K)
                FTHE(N)  = FTHE(N)  + FTHESKY(K)
                MS_2D(N) = MS_2D(N)  + FSKYM(K)
              ENDDO
            ENDDO
!$OMP END DO
          ! --------------
          ELSE
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
            DO N = 1,NUMNOD
              NCT = ADDCNE(N)-1
              NC = ADDCNE(N+1)-ADDCNE(N)
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
                FTHE(N)  = FTHE(N)  + FTHESKY(K)
              ENDDO
            ENDDO
!$OMP END DO
          ENDIF
          ! --------------
        ENDIF
        ! --------------
      ENDIF
      ! ------------------------------ 

      ! ------------------------------ 
      ! simplified flow
      IF(IALELAG > 0) THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
        DO N = 1,NUMNOD
          NCT = ADDCNE(N)-1
          NC = ADDCNE(N+1)-ADDCNE(N)
          DO K = NCT+1, NCT+NC
            AF(1,N)  = AF(1,N)   + FFSKY(1,K)
            AF(2,N)  = AF(2,N)   + FFSKY(2,K)
            AF(3,N)  = AF(3,N)   + FFSKY(3,K)
            MSNF(N)  = MSNF(N)   + FSKYM(K)
          ENDDO
        ENDDO
!$OMP END DO
      ENDIF
      ! ------------------------------ 

C-----------------------------------------------
C     SUPPRESS MASS DELETED SOLIDS (SOLIDS to SPH)
C-----------------------------------------------
      IF(SOL2SPH_FLAG/=0)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
        DO N = 1,NUMNOD
          NCT = ADDCNE(N)-1
          NC = ADDCNE(N+1)-ADDCNE(N)
          DO K = NCT+1, NCT+NC
            DMSPH(N) = DMSPH(N) + FSKYD(K)
          ENDDO
        ENDDO
!$OMP END DO
      END IF
C-----------------------------------------------
C     FORCES D'ELEMENTS  by ply
C-----------------------------------------------

      IF(IPLYXFEM > 0) THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
        DO N = 1,NUMNOD
          IL = INOD_PXFEM(N)
          IF(IL > 0) THEN
            NCT = ADDCNE_PXFEM(IL) - 1
            NC =  ADDCNE_PXFEM(IL+1) - ADDCNE_PXFEM(IL)
            DO K = NCT+1, NCT+NC
              DO J=1,NPLYMAX
                PLY(J)%A(1,IL) = PLY(J)%A(1,IL) + PLYSKY(J)%FSKY(1,K)
                PLY(J)%A(2,IL) = PLY(J)%A(2,IL) + PLYSKY(J)%FSKY(2,K)
                PLY(J)%A(3,IL) = PLY(J)%A(3,IL) + PLYSKY(J)%FSKY(3,K)
                PLY(J)%A(4,IL) = PLY(J)%A(4,IL) + PLYSKY(J)%FSKY(4,K)
              ENDDO
            ENDDO
          ENDIF
        ENDDO
!$OMP END DO
      ENDIF

      IF( (N2D/=0).OR.(ALE%SUB%IFSUBM==1).OR.(IALELAG > 0) ) CALL MY_BARRIER

C masse stocke dans FSKY(1) et A(1) utilise pour cumul en 2D
      IF(N2D/=0) THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
        DO I = 1,NUMNOD
          MS(I) = A(1,I)
          A(1,I) = ZERO
        ENDDO
!$OMP END DO
      ELSEIF(ALE%SUB%IFSUBM==1)THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
        DO I = 1,NUMNOD
          MS(I) = MS(I) + MSNF(I)
        ENDDO
!$OMP END DO
      ENDIF
C
      IF(IALELAG > 0) THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
        DO I = 1,NUMNOD
          MSF(I) = MSF(I) + MSNF(I)
        ENDDO
!$OMP END DO
      ENDIF
C-----------------------------------------------
C ITET=2 restore elementary STIFN of ND(middle node)
C-----------------------------------------------
      IF(NS10E>0) THEN
        CALL MY_BARRIER
!$OMP SINGLE
#include "vectorize.inc"
        DO I=1,NS10E
          ND  = IABS(ICNDS10(1,I))
          STIFND(I) = STIFN(ND)
        ENDDO
!$OMP END SINGLE
      ENDIF
C-----------------------------------------------
C Partie NITSCHE ELEM FORNEQS
C-----------------------------------------------
      IF(NITSCHE /= 0) THEN
!$OMP DO SCHEDULE(dynamic,CHUNK_NODE)
        DO N = 1,NUMNOD !NODFT, NODLT
          NCT = ADDCNE(N)-1
          NC = ADDCNE(N+1)-ADDCNE(N)
          DO K = NCT+1, NCT+NC
            DO NF=1,NFACNIT
              FORNEQS(1,N) = FORNEQS(1,N) + FORNEQSKY(3*(NF-1)+1,K)
              FORNEQS(2,N) = FORNEQS(2,N) + FORNEQSKY(3*(NF-1)+2,K)
              FORNEQS(3,N) = FORNEQS(3,N) + FORNEQSKY(3*(NF-1)+3,K)
            ENDDO
          ENDDO
        ENDDO
!$OMP END DO
      ENDIF

C-----------------------------------------------
C Partie INTERFACE
C-----------------------------------------------
      IF(NISKY>LSKYI)THEN
        WRITE(IOUT,*) ' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        WRITE(IOUT,*)
     .    ' PLEASE, INCREASE MULTIMP FOR INTERFACES 7, 10 AND 11'
        WRITE(ISTDO,*)' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
        TSTOP=ZERO

        RETURN
      ENDIF
C
      IF (NISKY/=0) THEN
        NISKYFT   = 1+ITASK*NISKY/ NTHREAD
        NISKYLT   = (ITASK+1)*NISKY/NTHREAD
        NFSKYFT   = 1+ITASK*NFSKYI/ NTHREAD
        NFSKYLT   = (ITASK+1)*NFSKYI/NTHREAD

        IF(INTPLYXFEM > 0 ) THEN
          NFSKYFT_INTPLY   = 1+ITASK*5/ NTHREAD
          NFSKYLT_INTPLY   = (ITASK+1)*5/NTHREAD
        ENDIF
C-----------------------------------------------
C Partie //
C-----------------------------------------------
        DO L=1,NFSKYI
!$OMP DO SCHEDULE(guided,CHUNK_ISKY)
          DO I=1,NISKY
            J = ISKY(I) ! address for N per contribution
            FSKY_L(J) = FSKYI(I,L) ! FSKYT : contribution triée par noeud
          END DO
!$OMP END DO
          call my_barrier()
!$OMP DO SCHEDULE(guided,CHUNK_ISKY)
          DO I=1,NISKY
            FSKYI(I,L) = FSKY_L(I)
          END DO
!$OMP END DO
        ENDDO
C
        IF(INTHEAT > 0 ) THEN
!$OMP SINGLE
          DO I=1,NISKY
            J = ISKY(I)
            FSKYT(J) = FTHESKYI(I)
          ENDDO
          DO  I=1,NISKY
            FTHESKYI(I) = FSKYT(I)
          ENDDO
!$OMP END SINGLE
          IF(NODADT_THERM ==1 ) THEN
!$OMP SINGLE
            DO I=1,NISKY
              J = ISKY(I)
              FSKYT(J) = CONDNSKYI(I)
            ENDDO
            DO  I=1,NISKY
              CONDNSKYI(I) = FSKYT(I)
            ENDDO
!$OMP END SINGLE
          ENDIF
        ENDIF
C
        IF(INTPLYXFEM > 0) THEN
          DO L = NFSKYFT_INTPLY,NFSKYLT_INTPLY
            DO I=1,NISKY
              J = ISKY(I)
              FSKYT(J) = PLYSKYI%FSKYI(I,L)
            END DO
            DO I=1,NISKY
              PLYSKYI%FSKYI(I,L) = FSKYT(I)
            END DO
          ENDDO
        ENDIF
C
C Barriere FSKYI et NISKY
C
        CALL MY_BARRIER
        NISKY = 0
C-----------------------------------------------
C     FORCES D'INTERFACES
C-----------------------------------------------

!$OMP DO SCHEDULE(dynamic,IVSIZE)
        DO NDFT = 1,NUMNOD,IVSIZE

          NDLT = MIN(NDFT+IVSIZE-1,NUMNOD)
          K1 = 0
          K2 = 0
          K3 = 0
          DO N=NDFT,NDLT
            NN = N-1
            DIFFADD = ADSKYI(N)-1-ADSKYI(NN)
            IF(DIFFADD==0) THEN
              K1 = K1 + 1
              VIND1(K1) = N
C   20 valeur seuil pour appel a qsort (20 = NBCC)
            ELSEIF(DIFFADD>=1.AND.DIFFADD<NBCC) THEN
              K2 = K2 + 1
              VIND2(K2) = N
            ELSEIF(DIFFADD>=NBCC) THEN
              K3 = K3 + 1
              VIND3(K3) = N
            ENDIF
          ENDDO
C
C une seule contribution
C
          IF(INTHEAT == 0 ) THEN
            IF(KDTINT==0)THEN
#include "vectorize.inc"
              DO IJK=1,K1
                N = VIND1(IJK)
                K=ADSKYI(N-1)
                A(1,N)  = A(1,N)  + FSKYI(K,1)
                A(2,N)  = A(2,N)  + FSKYI(K,2)
                A(3,N)  = A(3,N)  + FSKYI(K,3)
                STIFN(N) = STIFN(N) + FSKYI(K,4)
              ENDDO
            ELSE
#include "vectorize.inc"
              DO IJK=1,K1
                N = VIND1(IJK)
                K=ADSKYI(N-1)
                A(1,N)  = A(1,N)  + FSKYI(K,1)
                A(2,N)  = A(2,N)  + FSKYI(K,2)
                A(3,N)  = A(3,N)  + FSKYI(K,3)
                STIFN(N) = STIFN(N) + FSKYI(K,4)
                VISCN(N) = VISCN(N) + FSKYI(K,5)
              ENDDO
            ENDIF
C + la thermique
          ELSE
            IF(NODADT_THERM == 1) THEN
              IF(KDTINT==0)THEN
#include "vectorize.inc"
                DO IJK=1,K1
                  N = VIND1(IJK)
                  K=ADSKYI(N-1)
                  A(1,N)  = A(1,N)  + FSKYI(K,1)
                  A(2,N)  = A(2,N)  + FSKYI(K,2)
                  A(3,N)  = A(3,N)  + FSKYI(K,3)
                  STIFN(N) = STIFN(N) + FSKYI(K,4)
                  FTHE(N) = FTHE(N) + FTHESKYI(K)
                  CONDN(N) = CONDN(N) + CONDNSKYI(K)
                  FTHESKYI(K) = ZERO
                ENDDO
              ELSE
#include "vectorize.inc"
                DO IJK=1,K1
                  N = VIND1(IJK)
                  K=ADSKYI(N-1)
                  A(1,N)  = A(1,N)  + FSKYI(K,1)
                  A(2,N)  = A(2,N)  + FSKYI(K,2)
                  A(3,N)  = A(3,N)  + FSKYI(K,3)
                  STIFN(N) = STIFN(N) + FSKYI(K,4)
                  VISCN(N) = VISCN(N) + FSKYI(K,5)
                  FTHE(N) = FTHE(N) + FTHESKYI(K)
                  CONDN(N) = CONDN(N) + CONDNSKYI(K)
                  FTHESKYI(K) = ZERO
                ENDDO
              ENDIF
            ELSE
              IF(KDTINT==0)THEN
#include "vectorize.inc"
                DO IJK=1,K1
                  N = VIND1(IJK)
                  K=ADSKYI(N-1)
                  A(1,N)  = A(1,N)  + FSKYI(K,1)
                  A(2,N)  = A(2,N)  + FSKYI(K,2)
                  A(3,N)  = A(3,N)  + FSKYI(K,3)
                  STIFN(N) = STIFN(N) + FSKYI(K,4)
                  FTHE(N) = FTHE(N) + FTHESKYI(K)
                  FTHESKYI(K) = ZERO
                ENDDO
              ELSE
#include "vectorize.inc"
                DO IJK=1,K1
                  N = VIND1(IJK)
                  K=ADSKYI(N-1)
                  A(1,N)  = A(1,N)  + FSKYI(K,1)
                  A(2,N)  = A(2,N)  + FSKYI(K,2)
                  A(3,N)  = A(3,N)  + FSKYI(K,3)
                  STIFN(N) = STIFN(N) + FSKYI(K,4)
                  VISCN(N) = VISCN(N) + FSKYI(K,5)
                  FTHE(N) = FTHE(N) + FTHESKYI(K)
                  FTHESKYI(K) = ZERO
                ENDDO
              ENDIF
            ENDIF

          ENDIF
C
C  for plyxfem node
C

          IF(INTPLYXFEM > 0) THEN
#include "vectorize.inc"
            DO IJK=1,K1
              N = VIND1(IJK)
              K=ADSKYI(N-1)
              IL = INOD_PXFEM(N)
              IF(IL > 0) THEN
C                Iply is the position of ply in contact
                IPLY = INT(PLYSKYI%FSKYI(K,5))
                IF(IPLY > 0) THEN
                  PLY(IPLY)%A(1,IL)=PLY(IPLY)%A(1,IL) + PLYSKYI%FSKYI(K,1)
                  PLY(IPLY)%A(2,IL)=PLY(IPLY)%A(2,IL) + PLYSKYI%FSKYI(K,2)
                  PLY(IPLY)%A(3,IL)=PLY(IPLY)%A(3,IL) + PLYSKYI%FSKYI(K,3)
                  PLY(IPLY)%A(4,IL)=PLY(IPLY)%A(4,IL) + PLYSKYI%FSKYI(K,4)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
C
C pas plus de 20 contributions
C
          IF (IVECTOR==0) THEN
C   ANCIEN CODE
            DO 800 IJK=1,K2
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
  500           CONTINUE
C + la thermique
              ELSE
                IF(NODADT_THERM == 1 ) THEN
                  DO  K=JJ1,JJ2-1
                    DO  KK=K+1,JJ2
                      DO  L=1,NFSKYI
                        IF(FSKYI(KK,L)>FSKYI(K,L))THEN
                          FF = FSKYI(KK,L)
                          FSKYI(KK,L) = FSKYI(K,L)
                          FSKYI(K,L)  = FF
                        ENDIF
                      ENDDO
                      IF(FTHESKYI(KK)>FTHESKYI(K))THEN
                        FF = FTHESKYI(KK)
                        FTHESKYI(KK) = FTHESKYI(K)
                        FTHESKYI(K)  = FF
                      ENDIF
                      IF(CONDNSKYI(KK)>CONDNSKYI(K))THEN
                        FF = CONDNSKYI(KK)
                        CONDNSKYI(KK) = CONDNSKYI(K)
                        CONDNSKYI(K)  = FF
                      ENDIF
                    ENDDO
                  ENDDO
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
                      IF(FTHESKYI(KK)>FTHESKYI(K))THEN
                        FF = FTHESKYI(KK)
                        FTHESKYI(KK) = FTHESKYI(K)
                        FTHESKYI(K)  = FF
                      ENDIF
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF
C
              IF(INTPLYXFEM > 0 ) THEN
                DO  K=JJ1,JJ2-1
                  DO  KK=K+1,JJ2
                    IF(PLYSKYI%FSKYI(K,5) == PLYSKYI%FSKYI(KK,5)) THEN
                      DO  L=1,4
                        IF(PLYSKYI%FSKYI(KK,L)>PLYSKYI%FSKYI(K,L))THEN
                          FF = PLYSKYI%FSKYI(KK,L)
                          PLYSKYI%FSKYI(KK,L) = PLYSKYI%FSKYI(K,L)
                          PLYSKYI%FSKYI(K,L)  = FF
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
C
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
C+ la thermique
              ELSE
                IF(NODADT_THERM == 1) THEN
                  IF(KDTINT==0)THEN
                    DO K=JJ1,JJ2
                      A(1,N)  = A(1,N)  + FSKYI(K,1)
                      A(2,N)  = A(2,N)  + FSKYI(K,2)
                      A(3,N)  = A(3,N)  + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      FTHE(N)  = FTHE(N) + FTHESKYI(K)
                      CONDN(N) = CONDN(N)+ CONDNSKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ELSE
                    DO K=JJ1,JJ2
                      A(1,N)  = A(1,N)  + FSKYI(K,1)
                      A(2,N)  = A(2,N)  + FSKYI(K,2)
                      A(3,N)  = A(3,N)  + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      VISCN(N) = VISCN(N) + FSKYI(K,5)
                      FTHE(N)  = FTHE(N) + FTHESKYI(K)
                      CONDN(N) = CONDN(N)+ CONDNSKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ENDIF
                ELSE
                  IF(KDTINT==0)THEN
                    DO K=JJ1,JJ2
                      A(1,N)  = A(1,N)  + FSKYI(K,1)
                      A(2,N)  = A(2,N)  + FSKYI(K,2)
                      A(3,N)  = A(3,N)  + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      FTHE(N)  = FTHE(N) + FTHESKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ELSE
                    DO K=JJ1,JJ2
                      A(1,N)  = A(1,N)  + FSKYI(K,1)
                      A(2,N)  = A(2,N)  + FSKYI(K,2)
                      A(3,N)  = A(3,N)  + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      VISCN(N) = VISCN(N) + FSKYI(K,5)
                      FTHE(N)  = FTHE(N) + FTHESKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ENDIF
                ENDIF

              ENDIF
C
C  for plyxfem node
C

              IF(INTPLYXFEM > 0) THEN
                DO K=JJ1,JJ2
                  IL = INOD_PXFEM(N)
                  IF(IL > 0) THEN
C                   Iply is the position of ply in contact
                    IPLY = INT(PLYSKYI%FSKYI(K,5))
                    IF(IPLY > 0) THEN
                      PLY(IPLY)%A(1,IL)=PLY(IPLY)%A(1,IL)+ PLYSKYI%FSKYI(K,1)
                      PLY(IPLY)%A(2,IL)=PLY(IPLY)%A(2,IL)+ PLYSKYI%FSKYI(K,2)
                      PLY(IPLY)%A(3,IL)=PLY(IPLY)%A(3,IL)+ PLYSKYI%FSKYI(K,3)
                      PLY(IPLY)%A(4,IL)=PLY(IPLY)%A(4,IL)+ PLYSKYI%FSKYI(K,4)
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
C

C
  800       CONTINUE
          ELSE
C --- START-UP
C     DECIDE WHICH TREATMENT SHALL BE USED
C       USE OLD TREATMENT FOR NEWN < 100
C       USE NEW TREATMENT AND DECIDE WHERE TO USE NEW LOOP
C           AND WHERE TO STAY WITH OLD LOOP
            IF (K2<100) THEN
              KSPLIT=1
              DO I=1,K2
                IARRAY(I)=I
              ENDDO
              DO I=1,NBCC+1
                IC(I)=1
              ENDDO
            ELSE
              KMAX=0
#include "vectorize.inc"
              DO I=1,K2
                N = VIND2(I)
                JJ1 = ADSKYI(N-1)
                JJ2 = ADSKYI(N)-1
                LJ=JJ2-JJ1+1
                IF(LJ>KMAX) KMAX=LJ
                NN_A(I)=LJ
              ENDDO

              CALL ORDER_V(K2,1,KMAX,NN_A,IARRAY,IC)
              IC(KMAX+1)=IC(KMAX)+1
C
              KSPLIT=1
              DO I=2,KMAX
                IF(K2-IC(I)>NBCC) KSPLIT=I
              ENDDO
            ENDIF
C --- ORDERING LOOP
            KKEND=IC(KSPLIT+1)-1
            DO K=1,KSPLIT-1
              DO KK=K+1,KSPLIT
                KKSTART=IC(KK)
#include "vectorize.inc"
                DO I=KKSTART,KKEND
                  IJK=IARRAY(I)
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
                ENDDO
              ENDDO
            ENDDO
C
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
C L=5
                    IF (FSKYI(JJ,5)>FSKYI(J,5)) THEN
                      FF=FSKYI(JJ,5)
                      FSKYI(JJ,5)=FSKYI(J,5)
                      FSKYI(J,5) =FF
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
C  + la thermique
            IF(INTHEAT > 0)THEN
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
                    IF (FTHESKYI(JJ)>FTHESKYI(J)) THEN
                      FF=FTHESKYI(JJ)
                      FTHESKYI(JJ)=FTHESKYI(J)
                      FTHESKYI(J) =FF
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
C  + Thermal Time Step
              IF(NODADT_THERM == 1)THEN
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
                      IF (CONDNSKYI(JJ)>CONDNSKYI(J)) THEN
                        FF=CONDNSKYI(JJ)
                        CONDNSKYI(JJ)=CONDNSKYI(J)
                        CONDNSKYI(J) =FF
                      ENDIF
                    ENDDO
                  ENDDO
                ENDDO
              ENDIF


            ENDIF
C
C plyxfem + type 24 interface
C
C --- ORDERING LOOP
            IF(INTPLYXFEM > 0) THEN
              KKEND=IC(KSPLIT+1)-1
              DO K=1,KSPLIT-1
                DO KK=K+1,KSPLIT
                  KKSTART=IC(KK)
#include "vectorize.inc"
                  DO I=KKSTART,KKEND
                    IJK=IARRAY(I)
                    N=VIND2(IJK)
                    J=ADSKYI(N-1)+K-1
                    JJ=ADSKYI(N-1)+KK-1
                    IF (PLYSKYI%FSKYI(JJ,5) == PLYSKYI%FSKYI(J,5)) THEN
C L=1
                      IF (PLYSKYI%FSKYI(JJ,1)>PLYSKYI%FSKYI(J,1)) THEN
                        FF=PLYSKYI%FSKYI(JJ,1)
                        PLYSKYI%FSKYI(JJ,1)=PLYSKYI%FSKYI(J,1)
                        PLYSKYI%FSKYI(J,1) =FF
                      ENDIF
C L=2
                      IF (PLYSKYI%FSKYI(JJ,2)>PLYSKYI%FSKYI(J,2)) THEN
                        FF=PLYSKYI%FSKYI(JJ,2)
                        PLYSKYI%FSKYI(JJ,2)=PLYSKYI%FSKYI(J,2)
                        PLYSKYI%FSKYI(J,2) =FF
                      ENDIF
C L=3
                      IF (PLYSKYI%FSKYI(JJ,3)>PLYSKYI%FSKYI(J,3)) THEN
                        FF=PLYSKYI%FSKYI(JJ,3)
                        PLYSKYI%FSKYI(JJ,3)=PLYSKYI%FSKYI(J,3)
                        PLYSKYI%FSKYI(J,3) =FF
                      ENDIF
C L=4
                      IF (PLYSKYI%FSKYI(JJ,4)>PLYSKYI%FSKYI(J,4)) THEN
                        FF=PLYSKYI%FSKYI(JJ,4)
                        PLYSKYI%FSKYI(JJ,4)=PLYSKYI%FSKYI(J,4)
                        PLYSKYI%FSKYI(J,4) =FF
                      ENDIF
                    ENDIF
                  ENDDO
                ENDDO
              ENDDO
            ENDIF
C
C --- FILL ARRAY A
            IF(INTHEAT == 0 ) THEN
              IF(KDTINT==0)THEN
                DO K=1,KSPLIT
                  KKSTART=IC(K)
#include "vectorize.inc"
                  DO I=KKSTART,KKEND
                    IJK=IARRAY(I)
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
                    IJK=IARRAY(I)
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
              IF(NODADT_THERM ==1) THEN
                IF(KDTINT==0)THEN
                  DO K=1,KSPLIT
                    KKSTART=IC(K)
#include "vectorize.inc"
                    DO I=KKSTART,KKEND
                      IJK=IARRAY(I)
                      N=VIND2(IJK)
                      J=ADSKYI(N-1)+K-1
                      A(1,N) = A(1,N) + FSKYI(J,1)
                      A(2,N) = A(2,N) + FSKYI(J,2)
                      A(3,N) = A(3,N) + FSKYI(J,3)
                      STIFN(N) = STIFN(N) + FSKYI(J,4)
                      FTHE(N) = FTHE(N) + FTHESKYI(J)
                      CONDN(N)= CONDN(N)+ CONDNSKYI(J)
                      FTHESKYI(J) = ZERO
                    ENDDO
                  ENDDO
                ELSE
                  DO K=1,KSPLIT
                    KKSTART=IC(K)
#include "vectorize.inc"
                    DO I=KKSTART,KKEND
                      IJK=IARRAY(I)
                      N=VIND2(IJK)
                      J=ADSKYI(N-1)+K-1
                      A(1,N) = A(1,N) + FSKYI(J,1)
                      A(2,N) = A(2,N) + FSKYI(J,2)
                      A(3,N) = A(3,N) + FSKYI(J,3)
                      STIFN(N) = STIFN(N) + FSKYI(J,4)
                      VISCN(N) = VISCN(N) + FSKYI(J,5)
                      FTHE(N) = FTHE(N) + FTHESKYI(J)
                      CONDN(N)= CONDN(N)+ CONDNSKYI(J)
                      FTHESKYI(J) = ZERO
                    ENDDO
                  ENDDO
                ENDIF
              ELSE
                IF(KDTINT==0)THEN
                  DO K=1,KSPLIT
                    KKSTART=IC(K)
#include "vectorize.inc"
                    DO I=KKSTART,KKEND
                      IJK=IARRAY(I)
                      N=VIND2(IJK)
                      J=ADSKYI(N-1)+K-1
                      A(1,N) = A(1,N) + FSKYI(J,1)
                      A(2,N) = A(2,N) + FSKYI(J,2)
                      A(3,N) = A(3,N) + FSKYI(J,3)
                      STIFN(N) = STIFN(N) + FSKYI(J,4)
                      FTHE(N) = FTHE(N) + FTHESKYI(J)
                      FTHESKYI(J) = ZERO
                    ENDDO
                  ENDDO
                ELSE
                  DO K=1,KSPLIT
                    KKSTART=IC(K)
#include "vectorize.inc"
                    DO I=KKSTART,KKEND
                      IJK=IARRAY(I)
                      N=VIND2(IJK)
                      J=ADSKYI(N-1)+K-1
                      A(1,N) = A(1,N) + FSKYI(J,1)
                      A(2,N) = A(2,N) + FSKYI(J,2)
                      A(3,N) = A(3,N) + FSKYI(J,3)
                      STIFN(N) = STIFN(N) + FSKYI(J,4)
                      VISCN(N) = VISCN(N) + FSKYI(J,5)
                      FTHE(N) = FTHE(N) + FTHESKYI(J)
                      FTHESKYI(J) = ZERO
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
C
            IF(INTPLYXFEM > 0 ) THEN
              DO K=1,KSPLIT
                KKSTART=IC(K)
#include "vectorize.inc"
                DO I=KKSTART,KKEND
                  IJK=IARRAY(I)
                  N=VIND2(IJK)
                  J=ADSKYI(N-1)+K-1
                  IL = INOD_PXFEM(N)
                  IF(IL > 0) THEN
C                Iply is the position of ply in contact
                    IPLY = INT(PLYSKYI%FSKYI(J,5))
                    IF(IPLY > 0) THEN
                      PLY(IPLY)%A(1,IL)=PLY(IPLY)%A(1,IL) + PLYSKYI%FSKYI(J,1)
                      PLY(IPLY)%A(2,IL)=PLY(IPLY)%A(2,IL) + PLYSKYI%FSKYI(J,2)
                      PLY(IPLY)%A(3,IL)=PLY(IPLY)%A(3,IL) + PLYSKYI%FSKYI(J,3)
                      PLY(IPLY)%A(4,IL)=PLY(IPLY)%A(4,IL) + PLYSKYI%FSKYI(J,4)
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
C
C --- DO THE REST IN OLD WAY - SUM UP ALL CONTRIBUTIONS PER NODE
            NSTART=IC(KSPLIT+1)
            DO 801 I=NSTART,K2
              IJK=IARRAY(I)
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
  501           CONTINUE
C + la thermique
              ELSE
                IF(NODADT_THERM == 1) THEN
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
                      IF(CONDNSKYI(KK)>CONDNSKYI(K)) THEN
                        FF=CONDNSKYI(KK)
                        CONDNSKYI(KK)=CONDNSKYI(K)
                        CONDNSKYI(K)=FF
                      ENDIF
                    ENDDO
                  ENDDO
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
              ENDIF
C plyxfem + type 24
              IF(INTPLYXFEM > 0 ) THEN
                DO  K=JJ1,JJ2-1
                  DO  KK=K+1,JJ2
                    IF(PLYSKYI%FSKYI(K,5) == PLYSKYI%FSKYI(KK,5)) THEN
                      DO  L=1,NFSKYI - 1
                        IF(PLYSKYI%FSKYI(KK,L)>PLYSKYI%FSKYI(K,L)) THEN
                          FF=PLYSKYI%FSKYI(KK,L)
                          PLYSKYI%FSKYI(KK,L)=PLYSKYI%FSKYI(K,L)
                          PLYSKYI%FSKYI(K,L)=FF
                        ENDIF
                      ENDDO
                    ENDIF
                  ENDDO
                ENDDO
              ENDIF
C
              IF(INTHEAT ==  0 ) THEN
                IF(KDTINT==0)THEN
                  DO K=JJ1,JJ2
                    A(1,N) = A(1,N) + FSKYI(K,1)
                    A(2,N) = A(2,N) + FSKYI(K,2)
                    A(3,N) = A(3,N) + FSKYI(K,3)
                    STIFN(N) = STIFN(N) + FSKYI(K,4)
                  ENDDO
                ELSE
                  DO K=JJ1,JJ2
                    A(1,N) = A(1,N) + FSKYI(K,1)
                    A(2,N) = A(2,N) + FSKYI(K,2)
                    A(3,N) = A(3,N) + FSKYI(K,3)
                    STIFN(N) = STIFN(N) + FSKYI(K,4)
                    VISCN(N) = VISCN(N) + FSKYI(K,5)
                  ENDDO
                ENDIF
C + la thermique
              ELSE
                IF(NODADT_THERM == 1) THEN
                  IF(KDTINT==0)THEN
                    DO K=JJ1,JJ2
                      A(1,N) = A(1,N) + FSKYI(K,1)
                      A(2,N) = A(2,N) + FSKYI(K,2)
                      A(3,N) = A(3,N) + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      FTHE(N) = FTHE(N) + FTHESKYI(K)
                      CONDN(N)= CONDN(N) + CONDNSKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ELSE
                    DO K=JJ1,JJ2
                      A(1,N) = A(1,N) + FSKYI(K,1)
                      A(2,N) = A(2,N) + FSKYI(K,2)
                      A(3,N) = A(3,N) + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      VISCN(N) = VISCN(N) + FSKYI(K,5)
                      FTHE(N) = FTHE(N) + FTHESKYI(K)
                      CONDN(N)= CONDN(N) + CONDNSKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ENDIF
                ELSE
                  IF(KDTINT==0)THEN
                    DO K=JJ1,JJ2
                      A(1,N) = A(1,N) + FSKYI(K,1)
                      A(2,N) = A(2,N) + FSKYI(K,2)
                      A(3,N) = A(3,N) + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      FTHE(N) = FTHE(N) + FTHESKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ELSE
                    DO K=JJ1,JJ2
                      A(1,N) = A(1,N) + FSKYI(K,1)
                      A(2,N) = A(2,N) + FSKYI(K,2)
                      A(3,N) = A(3,N) + FSKYI(K,3)
                      STIFN(N) = STIFN(N) + FSKYI(K,4)
                      VISCN(N) = VISCN(N) + FSKYI(K,5)
                      FTHE(N) = FTHE(N) + FTHESKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ENDIF
                ENDIF
              ENDIF
C  plyxfem + type 24
              IF(INTPLYXFEM > 0) THEN
                DO K=JJ1,JJ2
                  IL = INOD_PXFEM(N)
                  IF(IL > 0) THEN
C                Iply is the position of ply in contact
                    IPLY = INT(PLYSKYI%FSKYI(K,5))
                    IF(IPLY > 0) THEN
                      PLY(IPLY)%A(1,IL)=PLY(IPLY)%A(1,IL) + PLYSKYI%FSKYI(K,1)
                      PLY(IPLY)%A(2,IL)=PLY(IPLY)%A(2,IL) + PLYSKYI%FSKYI(K,2)
                      PLY(IPLY)%A(3,IL)=PLY(IPLY)%A(3,IL) + PLYSKYI%FSKYI(K,3)
                      PLY(IPLY)%A(4,IL)=PLY(IPLY)%A(4,IL) + PLYSKYI%FSKYI(K,4)
                    ENDIF
                  ENDIF
                ENDDO
              ENDIF
C
  801       CONTINUE
          ENDIF
C   FIN NOUVEAUX TRAITEMENTS VECTORIELS
C
C plus de 20 contributions
C
          DO IJK=1,K3
            N = VIND3(IJK)
            NN = N-1
            JJ1 = ADSKYI(NN)
            JJ2 = ADSKYI(N)-1
C-----------------------------------------------
C       TRI DES FORCES D'INTERFACES (QSORT)
C-----------------------------------------------
            CALL ASS2SORT(FSKYI,JJ1,JJ2,FSKYT,NFSKYI)
            IF(INTHEAT >  0) CALL ASS2SORT(FTHESKYI,JJ1,JJ2,FSKYT,1)
            IF(NODADT_THERM == 1) CALL ASS2SORT(CONDNSKYI,JJ1,JJ2,FSKYT,1)
            IF(INTPLYXFEM>0)
     .      CALL ASS2SORT_PXFEM(PLYSKYI%FSKYI,JJ1,JJ2,FSKYT,5)
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
              IF(NODADT_THERM ==1) THEN
                IF(KDTINT==0)THEN
                  DO K=JJ1,JJ2
                    A(1,N)  = A(1,N)  + FSKYI(K,1)
                    A(2,N)  = A(2,N)  + FSKYI(K,2)
                    A(3,N)  = A(3,N)  + FSKYI(K,3)
                    STIFN(N) = STIFN(N) + FSKYI(K,4)
                    FTHE(N) = FTHE(N) + FTHESKYI(K)
                    CONDN(N)= CONDN(N) + CONDNSKYI(K)
                    FTHESKYI(K) = ZERO
                  ENDDO
                ELSE
                  DO K=JJ1,JJ2
                    A(1,N)  = A(1,N)  + FSKYI(K,1)
                    A(2,N)  = A(2,N)  + FSKYI(K,2)
                    A(3,N)  = A(3,N)  + FSKYI(K,3)
                    STIFN(N) = STIFN(N) + FSKYI(K,4)
                    VISCN(N) = VISCN(N) + FSKYI(K,5)
                    FTHE(N) = FTHE(N) + FTHESKYI(K)
                    CONDN(N)= CONDN(N) + CONDNSKYI(K)
                    FTHESKYI(K) = ZERO
                  ENDDO
                ENDIF
              ELSE
                IF(KDTINT==0)THEN
                  DO K=JJ1,JJ2
                    A(1,N)  = A(1,N)  + FSKYI(K,1)
                    A(2,N)  = A(2,N)  + FSKYI(K,2)
                    A(3,N)  = A(3,N)  + FSKYI(K,3)
                    STIFN(N) = STIFN(N) + FSKYI(K,4)
                    FTHE(N) = FTHE(N) + FTHESKYI(K)
                    FTHESKYI(K) = ZERO
                  ENDDO
                ELSE
                  DO K=JJ1,JJ2
                    A(1,N)  = A(1,N)  + FSKYI(K,1)
                    A(2,N)  = A(2,N)  + FSKYI(K,2)
                    A(3,N)  = A(3,N)  + FSKYI(K,3)
                    STIFN(N) = STIFN(N) + FSKYI(K,4)
                    VISCN(N) = VISCN(N) + FSKYI(K,5)
                    FTHE(N) = FTHE(N) + FTHESKYI(K)
                    FTHESKYI(K) = ZERO
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
C type 24 + plyxfem
            IF(INTPLYXFEM > 0) THEN
              DO K=JJ1,JJ2
                IL = INOD_PXFEM(N)
                IF(IL > 0 ) THEN
                  IPLY = INT(PLYSKYI%FSKYI(K,5))
                  IF(IPLY > 0) THEN
                    PLY(IPLY)%A(1,IL)=PLY(IPLY)%A(1,IL) + PLYSKYI%FSKYI(K,1)
                    PLY(IPLY)%A(2,IL)=PLY(IPLY)%A(2,IL) + PLYSKYI%FSKYI(K,2)
                    PLY(IPLY)%A(3,IL)=PLY(IPLY)%A(3,IL) + PLYSKYI%FSKYI(K,3)
                    PLY(IPLY)%A(4,IL)=PLY(IPLY)%A(4,IL) + PLYSKYI%FSKYI(K,4)
                  ENDIF
                ENDIF
              ENDDO
            ENDIF
C
          ENDDO
        ENDDO
!$OMP END DO
c ftheskyi and condnskyi are nor all reset to zero
c
C fin nisky/=0
      ENDIF
C
      NUM7 = NPSAV*NPART
C
      KM = 0
      DO K=1,NTHREAD-1
        KM = KM + NUM7
#include "vectorize.inc"
        DO I=PARTFT,PARTLT
          PARTSAV(I) = PARTSAV(I) + PARTSAV(I+KM)
          PARTSAV(I+KM) = ZERO
        END DO
      END DO
C
      CALL MY_BARRIER
C
      NUM7 = NPSAV*NGPE
C
      KM = 0
      IF (NTHPART > 0) THEN
        DO K=1,NTHREAD-1
          KM = KM + NUM7
#include "vectorize.inc"
          DO I=GREFT,GRELT
            GRESAV(I)    = GRESAV(I) + GRESAV(I+KM)
            GRESAV(I+KM) = ZERO
          ENDDO
        ENDDO
      ENDIF
      CALL MY_BARRIER
C-----------------------------------------------
C
      DEALLOCATE(VIND1)
      DEALLOCATE(VIND2)
      DEALLOCATE(VIND3)

      RETURN
      END SUBROUTINE ASSPAR4
Chd|====================================================================
Chd|  ASSPARI2                      source/assembly/asspar4.F     
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE ASSPARI2(
     1  A    ,AR    ,STIFN  ,STIFR  ,MS  ,
     2  IN   ,FSKYI2,I2SIZE ,ADDCNI2,INDSKY,
     3  FTHESKYI2,FTHE,CONDNSKYI2,CONDN)
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
#include      "parit_c.inc"
#include      "scr18_c.inc"
      INTEGER MAXBLOCI2,NBLOCI2,NBVALI2,NBCOLI2
      PARAMETER (MAXBLOCI2=1000)
      COMMON/PTMPARI2/NBLOCI2,NBVALI2(1:MAXBLOCI2),NBCOLI2(1:MAXBLOCI2)
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER I2SIZE, ADDCNI2(*),INDSKY(*)
C     REAL
      my_real
     .        A(3,*)  ,AR(3,*), STIFN(*), STIFR(*), MS(*), IN(*),
     .        FSKYI2(I2SIZE,*),FTHESKYI2(*),FTHE(*),CONDNSKYI2(*),CONDN(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,K,L,N,NC,NCT,IDEB,LB,KK,K1,K2
C-----------------------------------------------
C Interface type2
C-----------------------------------------------
      IF (IVECTOR==1) THEN
        IDEB = 1
        DO LB = 1, NBLOCI2
          NC = NBCOLI2(LB)
          KK = NBVALI2(LB)
          IF (NC==1) THEN
            IF(INTHEAT == 0 ) THEN
              IF(IRODDL/=0)THEN
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)
                  AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)
                  AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)
                  AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)
                  IN(N)    = IN(N)    + FSKYI2(9,NCT)
                  STIFR(N) = STIFR(N) + FSKYI2(10,NCT)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM == 0) THEN
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(NC==2) THEN
            IF(INTHEAT == 0 ) THEN
              IF(IRODDL/=0)THEN
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
                  AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
                  AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
                  AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
                  IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
                  STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM == 0) THEN
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+1)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+1)
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+1)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT+1)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+1)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT+1)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(NC==3) THEN
            IF(INTHEAT == 0 ) THEN
              IF(IRODDL/=0)THEN
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)
                  AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                + FSKYI2(6,NCT+2)
                  AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                + FSKYI2(7,NCT+2)
                  AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                + FSKYI2(8,NCT+2)
                  IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                + FSKYI2(9,NCT+2)
                  STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                + FSKYI2(10,NCT+2)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM ==0 )THEN
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                 + FSKYI2(1,NCT+2)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                 + FSKYI2(2,NCT+2)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                 + FSKYI2(3,NCT+2)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                 + FSKYI2(4,NCT+2)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                 + FSKYI2(5,NCT+2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                 + FSKYI2(6,NCT+2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                 + FSKYI2(7,NCT+2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                 + FSKYI2(8,NCT+2)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                 + FSKYI2(9,NCT+2)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                 + FSKYI2(10,NCT+2)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                 + FTHESKYI2(NCT+2)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                 + FSKYI2(1,NCT+2)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                 + FSKYI2(2,NCT+2)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                 + FSKYI2(3,NCT+2)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                 + FSKYI2(4,NCT+2)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                 + FSKYI2(5,NCT+2)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                 + FTHESKYI2(NCT+2)
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                 + FSKYI2(1,NCT+2)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                 + FSKYI2(2,NCT+2)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                 + FSKYI2(3,NCT+2)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                 + FSKYI2(4,NCT+2)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                 + FSKYI2(5,NCT+2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                 + FSKYI2(6,NCT+2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                 + FSKYI2(7,NCT+2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                 + FSKYI2(8,NCT+2)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                 + FSKYI2(9,NCT+2)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                 + FSKYI2(10,NCT+2)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                 + FTHESKYI2(NCT+2)
                    CONDN(N) = CONDN(N)  + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                 + CONDNSKYI2(NCT+2)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                 + FSKYI2(1,NCT+2)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                 + FSKYI2(2,NCT+2)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                 + FSKYI2(3,NCT+2)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                 + FSKYI2(4,NCT+2)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                 + FSKYI2(5,NCT+2)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                 + FTHESKYI2(NCT+2)
                    CONDN(N) = CONDN(N)  + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                 + CONDNSKYI2(NCT+2)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(NC==4) THEN
            IF(INTHEAT == 0 ) THEN
              IF(IRODDL/=0)THEN
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
                  AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
                  AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
                  AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
                  IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
                  STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM == 0) THEN
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                 + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                 + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                 + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                 + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                 + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                 + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                 + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                 + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                 + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                 + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                 + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                 + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                 + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                 + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                 + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                 + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                  + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                  + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                  + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                  + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                  + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
                    CONDN(N)  = FTHE(N) + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
                    CONDN(N)  = FTHE(N) + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(NC==5) THEN
            IF(INTHEAT == 0 ) THEN
              IF(IRODDL/=0)THEN
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                + FSKYI2(1,NCT+4)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                + FSKYI2(2,NCT+4)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                + FSKYI2(3,NCT+4)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                + FSKYI2(4,NCT+4)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                + FSKYI2(5,NCT+4)
                  AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                + FSKYI2(6,NCT+4)
                  AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                + FSKYI2(7,NCT+4)
                  AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                + FSKYI2(8,NCT+4)
                  IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                + FSKYI2(9,NCT+4)
                  STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                + FSKYI2(10,NCT+4)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                + FSKYI2(1,NCT+4)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                + FSKYI2(2,NCT+4)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                + FSKYI2(3,NCT+4)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                + FSKYI2(4,NCT+4)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                + FSKYI2(5,NCT+4)
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM == 0 ) THEN
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                  + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                  + FSKYI2(6,NCT+4)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                  + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                  + FSKYI2(7,NCT+4)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                  + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                  + FSKYI2(8,NCT+4)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                  + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                  + FSKYI2(9,NCT+4)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                  + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                  + FSKYI2(10,NCT+4)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                  + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                  + FSKYI2(6,NCT+4)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                  + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                  + FSKYI2(7,NCT+4)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                  + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                  + FSKYI2(8,NCT+4)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                  + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                  + FSKYI2(9,NCT+4)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                  + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                  + FSKYI2(10,NCT+4)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
     +                                  + CONDNSKYI2(NCT+4)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT)  + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
     +                                  + CONDNSKYI2(NCT+4)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(NC==6) THEN
            IF(INTHEAT == 0 ) THEN
              IF(IRODDL/=0)THEN
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
                  AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
                  AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
                  AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
                  IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
                  STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM == 0 ) THEN
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                  + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                  + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                  + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                  + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                  + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                  + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                  + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                  + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                  + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                  + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                  + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                  + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                  + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                  + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                  + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                  + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                  + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                  + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                  + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                  + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
     +                                  + CONDNSKYI2(NCT+4)+ CONDNSKYI2(NCT+5)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
                    FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
                    CONDN(N) = CONDN(N) + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
     +                                  + CONDNSKYI2(NCT+4)+ CONDNSKYI2(NCT+5)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ELSEIF(NC==7) THEN

            IF(IRODDL/=0)THEN
#include "vectorize.inc"
              DO K1 = IDEB, IDEB+KK-1
                N = INDSKY(K1)
                NCT = ADDCNI2(N)
                A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                              + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                              + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                              + FSKYI2(1,NCT+6)
                A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                              + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                              + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                              + FSKYI2(2,NCT+6)
                A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                              + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                              + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                              + FSKYI2(3,NCT+6)
                MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                              + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                              + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                              + FSKYI2(4,NCT+6)
                STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                              + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                              + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                              + FSKYI2(5,NCT+6)
                AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                              + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                              + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
     +                              + FSKYI2(6,NCT+6)
                AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                              + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                              + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
     +                              + FSKYI2(7,NCT+6)
                AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                              + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                              + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
     +                              + FSKYI2(8,NCT+6)
                IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                              + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                              + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
     +                              + FSKYI2(9,NCT+6)
                STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                              + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                              + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
     +                              + FSKYI2(10,NCT+6)
              ENDDO
            ELSE
#include "vectorize.inc"
              DO K1 = IDEB, IDEB+KK-1
                N = INDSKY(K1)
                NCT = ADDCNI2(N)
                A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                              + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                              + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                              + FSKYI2(1,NCT+6)
                A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                              + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                              + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                              + FSKYI2(2,NCT+6)
                A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                              + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                              + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                              + FSKYI2(3,NCT+6)
                MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                              + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                              + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                              + FSKYI2(4,NCT+6)
                STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                              + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                              + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                              + FSKYI2(5,NCT+6)
              ENDDO
            ENDIF
          ELSEIF(NC==8) THEN
            IF(IRODDL/=0)THEN
#include "vectorize.inc"
              DO K1 = IDEB, IDEB+KK-1
                N = INDSKY(K1)
                NCT = ADDCNI2(N)
                A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                              + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                              + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                              + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
                A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                              + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                              + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                              + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
                A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                              + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                              + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                              + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
                MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                              + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                              + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                              + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
                STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                              + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                              + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                              + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
                AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                              + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                              + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
     +                              + FSKYI2(6,NCT+6)+ FSKYI2(6,NCT+7)
                AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                              + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                              + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
     +                              + FSKYI2(7,NCT+6)+ FSKYI2(7,NCT+7)
                AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                              + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                              + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
     +                              + FSKYI2(8,NCT+6)+ FSKYI2(8,NCT+7)
                IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                              + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                              + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
     +                              + FSKYI2(9,NCT+6)+ FSKYI2(9,NCT+7)
                STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                              + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                              + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
     +                              + FSKYI2(10,NCT+6)+ FSKYI2(10,NCT+7)
              ENDDO
            ELSE
#include "vectorize.inc"
              DO K1 = IDEB, IDEB+KK-1
                N = INDSKY(K1)
                NCT = ADDCNI2(N)
                A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                              + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                              + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                              + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
                A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                              + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                              + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                              + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
                A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                              + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                              + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                              + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
                MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                              + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                              + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                              + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
                STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                              + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                              + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                              + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
              ENDDO
            ENDIF
          ELSEIF(NC==9) THEN
            IF(IRODDL/=0)THEN
#include "vectorize.inc"
              DO K1 = IDEB, IDEB+KK-1
                N = INDSKY(K1)
                NCT = ADDCNI2(N)
                A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                              + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                              + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                              + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                              + FSKYI2(1,NCT+8)
                A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                              + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                              + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                              + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                              + FSKYI2(2,NCT+8)
                A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                              + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                              + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                              + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                              + FSKYI2(3,NCT+8)
                MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                              + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                              + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                              + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                              + FSKYI2(4,NCT+8)
                STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                              + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                              + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                              + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                              + FSKYI2(5,NCT+8)
                AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                              + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                              + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
     +                              + FSKYI2(6,NCT+6)+ FSKYI2(6,NCT+7)
     +                              + FSKYI2(6,NCT+8)
                AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                              + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                              + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
     +                              + FSKYI2(7,NCT+6)+ FSKYI2(7,NCT+7)
     +                              + FSKYI2(7,NCT+8)
                AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                              + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                              + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
     +                              + FSKYI2(8,NCT+6)+ FSKYI2(8,NCT+7)
     +                              + FSKYI2(8,NCT+8)
                IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                              + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                              + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
     +                              + FSKYI2(9,NCT+6)+ FSKYI2(9,NCT+7)
     +                              + FSKYI2(9,NCT+8)
                STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                              + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                              + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
     +                              + FSKYI2(10,NCT+6)+ FSKYI2(10,NCT+7)
     +                              + FSKYI2(10,NCT+8)
              ENDDO
            ELSE
#include "vectorize.inc"
              DO K1 = IDEB, IDEB+KK-1
                N = INDSKY(K1)
                NCT = ADDCNI2(N)
                A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                              + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                              + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                              + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                              + FSKYI2(1,NCT+8)
                A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                              + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                              + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                              + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                              + FSKYI2(2,NCT+8)
                A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                              + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                              + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                              + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                              + FSKYI2(3,NCT+8)
                MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                              + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                              + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                              + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                              + FSKYI2(4,NCT+8)
                STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                              + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                              + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                              + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                              + FSKYI2(5,NCT+8)
              ENDDO
            ENDIF
          ELSEIF(NC==10) THEN
            IF(INTHEAT == 0) THEN
              IF(IRODDL/=0)THEN
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                                + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                                + FSKYI2(1,NCT+8)+ FSKYI2(1,NCT+9)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                                + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                                + FSKYI2(2,NCT+8)+ FSKYI2(2,NCT+9)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                                + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                                + FSKYI2(3,NCT+8)+ FSKYI2(3,NCT+9)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                                + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                                + FSKYI2(4,NCT+8)+ FSKYI2(4,NCT+9)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                                + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                                + FSKYI2(5,NCT+8)+ FSKYI2(5,NCT+9)
                  AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
     +                                + FSKYI2(6,NCT+6)+ FSKYI2(6,NCT+7)
     +                                + FSKYI2(6,NCT+8)+ FSKYI2(6,NCT+9)
                  AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
     +                                + FSKYI2(7,NCT+6)+ FSKYI2(7,NCT+7)
     +                                + FSKYI2(7,NCT+8)+ FSKYI2(7,NCT+9)
                  AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
     +                                + FSKYI2(8,NCT+6)+ FSKYI2(8,NCT+7)
     +                                + FSKYI2(8,NCT+8)+ FSKYI2(8,NCT+9)
                  IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
     +                                + FSKYI2(9,NCT+6)+ FSKYI2(9,NCT+7)
     +                                + FSKYI2(9,NCT+8)+ FSKYI2(9,NCT+9)
                  STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
     +                                + FSKYI2(10,NCT+6)+ FSKYI2(10,NCT+7)
     +                                + FSKYI2(10,NCT+8)+ FSKYI2(10,NCT+9)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                                + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                                + FSKYI2(1,NCT+8)+ FSKYI2(1,NCT+9)
                  A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                                + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                                + FSKYI2(2,NCT+8)+ FSKYI2(2,NCT+9)
                  A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                                + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                                + FSKYI2(3,NCT+8)+ FSKYI2(3,NCT+9)
                  MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                                + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                                + FSKYI2(4,NCT+8)+ FSKYI2(4,NCT+9)
                  STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                                + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                                + FSKYI2(5,NCT+8)+ FSKYI2(5,NCT+9)
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM == 0) THEN
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                                  + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                                  + FSKYI2(1,NCT+8)+ FSKYI2(1,NCT+9)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                                  + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                                  + FSKYI2(2,NCT+8)+ FSKYI2(2,NCT+9)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                                  + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                                  + FSKYI2(3,NCT+8)+ FSKYI2(3,NCT+9)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                                  + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                                  + FSKYI2(4,NCT+8)+ FSKYI2(4,NCT+9)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                                  + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                                  + FSKYI2(5,NCT+8)+ FSKYI2(5,NCT+9)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                  + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                  + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
     +                                  + FSKYI2(6,NCT+6)+ FSKYI2(6,NCT+7)
     +                                  + FSKYI2(6,NCT+8)+ FSKYI2(6,NCT+9)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                  + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                  + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
     +                                  + FSKYI2(7,NCT+6)+ FSKYI2(7,NCT+7)
     +                                  + FSKYI2(7,NCT+8)+ FSKYI2(7,NCT+9)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                  + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                  + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
     +                                  + FSKYI2(8,NCT+6)+ FSKYI2(8,NCT+7)
     +                                  + FSKYI2(8,NCT+8)+ FSKYI2(8,NCT+9)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                  + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                  + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
     +                                  + FSKYI2(9,NCT+6)+ FSKYI2(9,NCT+7)
     +                                  + FSKYI2(9,NCT+8)+ FSKYI2(9,NCT+9)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                  + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                  + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
     +                                  + FSKYI2(10,NCT+6)+ FSKYI2(10,NCT+7)
     +                                  + FSKYI2(10,NCT+8)+ FSKYI2(10,NCT+9)
                    FTHE(N) = FTHE(N)   + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
     +                                  + FTHESKYI2(NCT+6)+ FTHESKYI2(NCT+7)
     +                                  + FTHESKYI2(NCT+8)+ FTHESKYI2(NCT+9)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                                  + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                                  + FSKYI2(1,NCT+8)+ FSKYI2(1,NCT+9)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                                  + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                                  + FSKYI2(2,NCT+8)+ FSKYI2(2,NCT+9)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                                  + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                                  + FSKYI2(3,NCT+8)+ FSKYI2(3,NCT+9)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                                  + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                                  + FSKYI2(4,NCT+8)+ FSKYI2(4,NCT+9)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                                  + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                                  + FSKYI2(5,NCT+8)+ FSKYI2(5,NCT+9)
                    FTHE(N) = FTHE(N)   + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
     +                                  + FTHESKYI2(NCT+6)+ FTHESKYI2(NCT+7)
     +                                  + FTHESKYI2(NCT+8)+ FTHESKYI2(NCT+9)
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                                  + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                                  + FSKYI2(1,NCT+8)+ FSKYI2(1,NCT+9)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                                  + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                                  + FSKYI2(2,NCT+8)+ FSKYI2(2,NCT+9)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                                  + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                                  + FSKYI2(3,NCT+8)+ FSKYI2(3,NCT+9)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                                  + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                                  + FSKYI2(4,NCT+8)+ FSKYI2(4,NCT+9)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                                  + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                                  + FSKYI2(5,NCT+8)+ FSKYI2(5,NCT+9)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT)  + FSKYI2(6,NCT+1)
     +                                  + FSKYI2(6,NCT+2)+ FSKYI2(6,NCT+3)
     +                                  + FSKYI2(6,NCT+4)+ FSKYI2(6,NCT+5)
     +                                  + FSKYI2(6,NCT+6)+ FSKYI2(6,NCT+7)
     +                                  + FSKYI2(6,NCT+8)+ FSKYI2(6,NCT+9)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT)  + FSKYI2(7,NCT+1)
     +                                  + FSKYI2(7,NCT+2)+ FSKYI2(7,NCT+3)
     +                                  + FSKYI2(7,NCT+4)+ FSKYI2(7,NCT+5)
     +                                  + FSKYI2(7,NCT+6)+ FSKYI2(7,NCT+7)
     +                                  + FSKYI2(7,NCT+8)+ FSKYI2(7,NCT+9)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT)  + FSKYI2(8,NCT+1)
     +                                  + FSKYI2(8,NCT+2)+ FSKYI2(8,NCT+3)
     +                                  + FSKYI2(8,NCT+4)+ FSKYI2(8,NCT+5)
     +                                  + FSKYI2(8,NCT+6)+ FSKYI2(8,NCT+7)
     +                                  + FSKYI2(8,NCT+8)+ FSKYI2(8,NCT+9)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT)  + FSKYI2(9,NCT+1)
     +                                  + FSKYI2(9,NCT+2)+ FSKYI2(9,NCT+3)
     +                                  + FSKYI2(9,NCT+4)+ FSKYI2(9,NCT+5)
     +                                  + FSKYI2(9,NCT+6)+ FSKYI2(9,NCT+7)
     +                                  + FSKYI2(9,NCT+8)+ FSKYI2(9,NCT+9)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT) + FSKYI2(10,NCT+1)
     +                                  + FSKYI2(10,NCT+2)+ FSKYI2(10,NCT+3)
     +                                  + FSKYI2(10,NCT+4)+ FSKYI2(10,NCT+5)
     +                                  + FSKYI2(10,NCT+6)+ FSKYI2(10,NCT+7)
     +                                  + FSKYI2(10,NCT+8)+ FSKYI2(10,NCT+9)
                    FTHE(N) = FTHE(N)   + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
     +                                  + FTHESKYI2(NCT+6)+ FTHESKYI2(NCT+7)
     +                                  + FTHESKYI2(NCT+8)+ FTHESKYI2(NCT+9)
                    CONDN(N)= CONDN(N)  + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
     +                                  + CONDNSKYI2(NCT+4)+ CONDNSKYI2(NCT+5)
     +                                  + CONDNSKYI2(NCT+6)+ CONDNSKYI2(NCT+7)
     +                                  + CONDNSKYI2(NCT+8)+ CONDNSKYI2(NCT+9)
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT)  + FSKYI2(1,NCT+1)
     +                                  + FSKYI2(1,NCT+2)+ FSKYI2(1,NCT+3)
     +                                  + FSKYI2(1,NCT+4)+ FSKYI2(1,NCT+5)
     +                                  + FSKYI2(1,NCT+6)+ FSKYI2(1,NCT+7)
     +                                  + FSKYI2(1,NCT+8)+ FSKYI2(1,NCT+9)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT)  + FSKYI2(2,NCT+1)
     +                                  + FSKYI2(2,NCT+2)+ FSKYI2(2,NCT+3)
     +                                  + FSKYI2(2,NCT+4)+ FSKYI2(2,NCT+5)
     +                                  + FSKYI2(2,NCT+6)+ FSKYI2(2,NCT+7)
     +                                  + FSKYI2(2,NCT+8)+ FSKYI2(2,NCT+9)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT)  + FSKYI2(3,NCT+1)
     +                                  + FSKYI2(3,NCT+2)+ FSKYI2(3,NCT+3)
     +                                  + FSKYI2(3,NCT+4)+ FSKYI2(3,NCT+5)
     +                                  + FSKYI2(3,NCT+6)+ FSKYI2(3,NCT+7)
     +                                  + FSKYI2(3,NCT+8)+ FSKYI2(3,NCT+9)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT)  + FSKYI2(4,NCT+1)
     +                                  + FSKYI2(4,NCT+2)+ FSKYI2(4,NCT+3)
     +                                  + FSKYI2(4,NCT+4)+ FSKYI2(4,NCT+5)
     +                                  + FSKYI2(4,NCT+6)+ FSKYI2(4,NCT+7)
     +                                  + FSKYI2(4,NCT+8)+ FSKYI2(4,NCT+9)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT)  + FSKYI2(5,NCT+1)
     +                                  + FSKYI2(5,NCT+2)+ FSKYI2(5,NCT+3)
     +                                  + FSKYI2(5,NCT+4)+ FSKYI2(5,NCT+5)
     +                                  + FSKYI2(5,NCT+6)+ FSKYI2(5,NCT+7)
     +                                  + FSKYI2(5,NCT+8)+ FSKYI2(5,NCT+9)
                    FTHE(N) = FTHE(N)   + FTHESKYI2(NCT) + FTHESKYI2(NCT+1)
     +                                  + FTHESKYI2(NCT+2)+ FTHESKYI2(NCT+3)
     +                                  + FTHESKYI2(NCT+4)+ FTHESKYI2(NCT+5)
     +                                  + FTHESKYI2(NCT+6)+ FTHESKYI2(NCT+7)
     +                                  + FTHESKYI2(NCT+8)+ FTHESKYI2(NCT+9)
                    CONDN(N)= CONDN(N)  + CONDNSKYI2(NCT) + CONDNSKYI2(NCT+1)
     +                                  + CONDNSKYI2(NCT+2)+ CONDNSKYI2(NCT+3)
     +                                  + CONDNSKYI2(NCT+4)+ CONDNSKYI2(NCT+5)
     +                                  + CONDNSKYI2(NCT+6)+ CONDNSKYI2(NCT+7)
     +                                  + CONDNSKYI2(NCT+8)+ CONDNSKYI2(NCT+9)
                  ENDDO
                ENDIF
              ENDIF
            ENDIF

          ELSE

            IF(INTHEAT == 0) THEN
              IF(IRODDL/=0)THEN
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  DO K2 = 0, NC-1
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT+K2)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT+K2)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT+K2)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT+K2)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT+K2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT+K2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT+K2)
                    AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT+K2)
                    IN(N)    = IN(N)    + FSKYI2(9,NCT+K2)
                    STIFR(N) = STIFR(N) + FSKYI2(10,NCT+K2)
                  ENDDO
                ENDDO
              ELSE
                DO K1 = IDEB, IDEB+KK-1
                  N = INDSKY(K1)
                  NCT = ADDCNI2(N)
                  DO K2 = 0, NC-1
                    A(1,N)   = A(1,N)   + FSKYI2(1,NCT+K2)
                    A(2,N)   = A(2,N)   + FSKYI2(2,NCT+K2)
                    A(3,N)   = A(3,N)   + FSKYI2(3,NCT+K2)
                    MS(N)    = MS(N)    + FSKYI2(4,NCT+K2)
                    STIFN(N) = STIFN(N) + FSKYI2(5,NCT+K2)
                  ENDDO
                ENDDO
              ENDIF
            ELSE
              IF(IDT_THERM == 0) THEN
                IF(IRODDL/=0)THEN
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    DO K2 = 0, NC-1
                      A(1,N)   = A(1,N)   + FSKYI2(1,NCT+K2)
                      A(2,N)   = A(2,N)   + FSKYI2(2,NCT+K2)
                      A(3,N)   = A(3,N)   + FSKYI2(3,NCT+K2)
                      MS(N)    = MS(N)    + FSKYI2(4,NCT+K2)
                      STIFN(N) = STIFN(N) + FSKYI2(5,NCT+K2)
                      AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT+K2)
                      AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT+K2)
                      AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT+K2)
                      IN(N)    = IN(N)    + FSKYI2(9,NCT+K2)
                      STIFR(N) = STIFR(N) + FSKYI2(10,NCT+K2)
                      FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+K2)
                    ENDDO
                  ENDDO
                ELSE
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    DO K2 = 0, NC-1
                      A(1,N)   = A(1,N)   + FSKYI2(1,NCT+K2)
                      A(2,N)   = A(2,N)   + FSKYI2(2,NCT+K2)
                      A(3,N)   = A(3,N)   + FSKYI2(3,NCT+K2)
                      MS(N)    = MS(N)    + FSKYI2(4,NCT+K2)
                      STIFN(N) = STIFN(N) + FSKYI2(5,NCT+K2)
                      FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+K2)
                    ENDDO
                  ENDDO
                ENDIF
              ELSE
                IF(IRODDL/=0)THEN
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    DO K2 = 0, NC-1
                      A(1,N)   = A(1,N)   + FSKYI2(1,NCT+K2)
                      A(2,N)   = A(2,N)   + FSKYI2(2,NCT+K2)
                      A(3,N)   = A(3,N)   + FSKYI2(3,NCT+K2)
                      MS(N)    = MS(N)    + FSKYI2(4,NCT+K2)
                      STIFN(N) = STIFN(N) + FSKYI2(5,NCT+K2)
                      AR(1,N)  = AR(1,N)  + FSKYI2(6,NCT+K2)
                      AR(1,N)  = AR(1,N)  + FSKYI2(7,NCT+K2)
                      AR(1,N)  = AR(1,N)  + FSKYI2(8,NCT+K2)
                      IN(N)    = IN(N)    + FSKYI2(9,NCT+K2)
                      STIFR(N) = STIFR(N) + FSKYI2(10,NCT+K2)
                      FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+K2)
                      CONDN(N) = CONDN(N) + CONDNSKYI2(NCT+K2)
                    ENDDO
                  ENDDO
                ELSE
                  DO K1 = IDEB, IDEB+KK-1
                    N = INDSKY(K1)
                    NCT = ADDCNI2(N)
                    DO K2 = 0, NC-1
                      A(1,N)   = A(1,N)   + FSKYI2(1,NCT+K2)
                      A(2,N)   = A(2,N)   + FSKYI2(2,NCT+K2)
                      A(3,N)   = A(3,N)   + FSKYI2(3,NCT+K2)
                      MS(N)    = MS(N)    + FSKYI2(4,NCT+K2)
                      STIFN(N) = STIFN(N) + FSKYI2(5,NCT+K2)
                      FTHE(N)  = FTHE(N)  + FTHESKYI2(NCT+K2)
                      CONDN(N) = CONDN(N) + CONDNSKYI2(NCT+K2)
                    ENDDO
                  ENDDO
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          IDEB = IDEB+KK
        ENDDO
C
      ELSE
C
C scalaire
        IF(INTHEAT == 0 ) THEN
          IF(IRODDL/=0)THEN
            DO N = 1, NUMNOD
              NCT = ADDCNI2(N)-1
              NC = ADDCNI2(N+1)-ADDCNI2(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKYI2(1,K)
                A(2,N)   = A(2,N)   + FSKYI2(2,K)
                A(3,N)   = A(3,N)   + FSKYI2(3,K)
                MS(N)    = MS(N)    + FSKYI2(4,K)
                STIFN(N) = STIFN(N) + FSKYI2(5,K)
                AR(1,N)  = AR(1,N)  + FSKYI2(6,K)
                AR(2,N)  = AR(2,N)  + FSKYI2(7,K)
                AR(3,N)  = AR(3,N)  + FSKYI2(8,K)
                IN(N)    = IN(N)    + FSKYI2(9,K)
                STIFR(N) = STIFR(N) + FSKYI2(10,K)
              ENDDO
            ENDDO
          ELSE
            DO N = 1, NUMNOD
              NCT = ADDCNI2(N)-1
              NC = ADDCNI2(N+1)-ADDCNI2(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKYI2(1,K)
                A(2,N)   = A(2,N)   + FSKYI2(2,K)
                A(3,N)   = A(3,N)   + FSKYI2(3,K)
                MS(N)    = MS(N)    + FSKYI2(4,K)
                STIFN(N) = STIFN(N) + FSKYI2(5,K)
              ENDDO
            ENDDO
          ENDIF
        ELSE
          IF(IRODDL/=0)THEN
            DO N = 1, NUMNOD
              NCT = ADDCNI2(N)-1
              NC = ADDCNI2(N+1)-ADDCNI2(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKYI2(1,K)
                A(2,N)   = A(2,N)   + FSKYI2(2,K)
                A(3,N)   = A(3,N)   + FSKYI2(3,K)
                MS(N)    = MS(N)    + FSKYI2(4,K)
                STIFN(N) = STIFN(N) + FSKYI2(5,K)
                AR(1,N)  = AR(1,N)  + FSKYI2(6,K)
                AR(2,N)  = AR(2,N)  + FSKYI2(7,K)
                AR(3,N)  = AR(3,N)  + FSKYI2(8,K)
                IN(N)    = IN(N)    + FSKYI2(9,K)
                STIFR(N) = STIFR(N) + FSKYI2(10,K)
                FTHE(N) = FTHE(N) + FTHESKYI2(K)
              ENDDO
            ENDDO
          ELSE
            DO N = 1, NUMNOD
              NCT = ADDCNI2(N)-1
              NC = ADDCNI2(N+1)-ADDCNI2(N)
              DO K = NCT+1, NCT+NC
                A(1,N)   = A(1,N)   + FSKYI2(1,K)
                A(2,N)   = A(2,N)   + FSKYI2(2,K)
                A(3,N)   = A(3,N)   + FSKYI2(3,K)
                MS(N)    = MS(N)    + FSKYI2(4,K)
                STIFN(N) = STIFN(N) + FSKYI2(5,K)
                FTHE(N) = FTHE(N) + FTHESKYI2(K)
              ENDDO
            ENDDO
          ENDIF
        ENDIF

      ENDIF
C-----------------------------------------------
C
      RETURN
      END
