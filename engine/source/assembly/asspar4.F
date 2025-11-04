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
!||    asspar4            ../engine/source/assembly/asspar4.F
!||--- called by ------------------------------------------------------
!||    resol              ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    ass2sort           ../engine/source/assembly/ass2sort.F
!||    ass2sort_pxfem     ../engine/source/assembly/ass2sort.F
!||    my_barrier         ../engine/source/system/machine.F
!||--- uses       -----------------------------------------------------
!||    ale_mod            ../common_source/modules/ale/ale_mod.F
!||    connectivity_mod   ../common_source/modules/connectivity.F90
!||    glob_therm_mod     ../common_source/modules/mat_elem/glob_therm_mod.F90
!||    nodal_arrays_mod   ../common_source/modules/nodal_arrays.F90
!||    plyxfem_mod        ../engine/share/modules/plyxfem_mod.F
!||====================================================================
      SUBROUTINE ASSPAR4(NODES,
     2  FSKY    ,FSKYV   ,ADSKY , FSKYM   ,
     3  MSNF    ,ISKY    ,FSKYI  ,FTHE    ,
     4  FTHESKY ,FTHESKYI,NODFT  ,NODLT  ,ADSKYI  ,
     5  PARTSAV ,PARTFT  ,PARTLT ,ITASK  ,GREFT   ,
     6  GRELT   ,GRESAV  ,AF     ,FFSKY  ,MSF     ,
     7  ADSKY_PXFEM ,INOD_PXFEM ,FSKYD   ,
     8  DMSPH   ,CONDN,CONDNSKY,CONDNSKYI,MS_2D,ICNDS10 ,
     A  STIFND  ,FORNEQS ,FORNEQSKY,NFACNIT,NODFT_2,
     B  NODLT_2 ,FSKY_L  ,GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE nodal_arrays_mod
        USE connectivity_mod
        USE PLYXFEM_MOD
        USE ALE_MOD
        use glob_therm_mod
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
        TYPE(nodal_arrays_), intent(inout) :: NODES
        INTEGER ADSKY(*), ISKY(*),
     .          ADSKYI(0:NUMNOD+1),
     .          NODFT, NODLT, ITASK, PARTFT, PARTLT,GREFT,GRELT,
     .          ADSKY_PXFEM(*),INOD_PXFEM(*), ICNDS10(3,*),NODFT_2  ,NODLT_2

C     REAL
        my_real
     .     FSKYV(LSKY,8),FSKY(8,LSKY),
     .     FSKYM(*), MSNF(*), FSKYI(LSKYI,NFSKYI),
     .     FTHE(*), FTHESKY(LSKY), FTHESKYI(LSKYI), PARTSAV(*),GRESAV(*),
     .     AF(3,*),FFSKY(3,*),MSF(*), FSKYD(*), DMSPH(*),CONDNSKY(LSKY),CONDN(*),
     .     CONDNSKYI(LSKYI),MS_2D(*),STIFND(*),FORNEQS(3,*) ,
     .     FORNEQSKY(3*NFACNIT,*)
        my_real, DIMENSION(NISKY), INTENT(INOUT) :: FSKY_L !< working array for FSKY sorting
        type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER VSIZE, NBCC, NUM7, KM,IL,IPLY,ND
        PARAMETER (VSIZE = 8192)
        PARAMETER (NBCC = 20)
        INTEGER I,J,L,K,N,NC,KK,JJ,JJ1,JJ2,NN,NFSKYFT_INTPLY,NFSKYLT_INTPLY,
     .          NISKYFT,NISKYLT,NFSKYFT,NFSKYLT,K1,K2,K3,IJK,NF,
     .          NCT,DIFFADD,NDLT,NDFT,LJ,KKEND,NSTART,KKSTART,KMAX,KSPLIT
        INTEGER IC(NBCC+1),NN_A(VSIZE),IARRAY(VSIZE)
        my_real FF, FSKYT(NISKY)
        INTEGER :: VIND_SIZE
        INTEGER, DIMENSION(:), ALLOCATABLE :: VIND1,VIND2,VIND3
        INTEGER :: CHUNK_NODE,CHUNK_NODE_1
        INTEGER, PARAMETER :: IVSIZE = 32
C-----------------------------------------------
C element
C-----------------------------------------------

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

!$OMP DO SCHEDULE(guided)
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
C Computing SKYLINE VECTOR ADDRESSES
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
        IF(GLOB_THERM%ITHERM_FE == 0 ) THEN
          ! --------------
          IF(ALE%SUB%IFSUBM==0)THEN
!$OMP DO SCHEDULE(guided)
            DO N = 1,NUMNOD
              NCT = ADSKY(N)-1
              NC = ADSKY(N+1)-ADSKY(N)
              DO K = NCT+1, NCT+NC ! ADSKY(N) to ADSKY(N+1)-1
                NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
              ENDDO
            ENDDO
!$OMP END DO
            ! --------------
          ELSEIF(N2D/=0)THEN
!$OMP DO SCHEDULE(guided)
            DO N = 1,NUMNOD
              NCT = ADSKY(N)-1
              NC = ADSKY(N+1)-ADSKY(N)
              DO K = NCT+1, NCT+NC
                NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
                MS_2D(N) = MS_2D(N)  + FSKYM(K)
              ENDDO
            ENDDO
!$OMP END DO
            ! --------------
          ELSE
!$OMP DO SCHEDULE(guided)
            DO N = 1,NUMNOD
              NCT = ADSKY(N)-1
              NC = ADSKY(N+1)-ADSKY(N)
              DO K = NCT+1, NCT+NC
                NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
                MSNF(N)  = MSNF(N)  + FSKYM(K)
              ENDDO
            ENDDO
!$OMP END DO
          ENDIF
          ! --------------
          ! ------------------------------
        ELSE
          ! --------------
          IF(GLOB_THERM%NODADT_THERM == 1) THEN
            ! --------------
            IF(ALE%SUB%IFSUBM==0)THEN
!$OMP DO SCHEDULE(guided)
              DO N = 1,NUMNOD
                NCT = ADSKY(N)-1
                NC = ADSKY(N+1)-ADSKY(N)
                DO K = NCT+1, NCT+NC
                  NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                  NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                  NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                  NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                  NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                  NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                  NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
                  FTHE(N)  = FTHE(N)  + FTHESKY(K)
                  CONDN(N) = CONDN(N) + CONDNSKY(K)
                ENDDO
              ENDDO
!$OMP END DO
              ! --------------
            ELSEIF(N2D/=0)THEN
!$OMP DO SCHEDULE(guided)
              DO N = 1,NUMNOD
                NCT = ADSKY(N)-1
                NC = ADSKY(N+1)-ADSKY(N)
                DO K = NCT+1, NCT+NC
                  NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                  NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                  NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                  NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                  NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                  NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                  NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
                  FTHE(N)  = FTHE(N)  + FTHESKY(K)
                  CONDN(N) = CONDN(N) + CONDNSKY(K)
                  MS_2D(N) = MS_2D(N)  + FSKYM(K)
                ENDDO
              ENDDO
!$OMP END DO
              ! --------------
            ELSE
!$OMP DO SCHEDULE(guided)
              DO N = 1,NUMNOD
                NCT = ADSKY(N)-1
                NC = ADSKY(N+1)-ADSKY(N)
                DO K = NCT+1, NCT+NC
                  NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                  NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                  NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                  NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                  NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                  NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                  NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
                  MSNF(N)  = MSNF(N)  + FSKYM(K)
                  FTHE(N)  = FTHE(N)  + FTHESKY(K)
                  CONDN(N) = CONDN(N) + CONDNSKY(K)
                ENDDO
              ENDDO
!$OMP END DO
            ENDIF
            ! --------------
            ! --------------
          ELSE ! GLOB_THERM%NODADT_THERM/=1
            ! --------------
            IF(ALE%SUB%IFSUBM==0)THEN
!$OMP DO SCHEDULE(guided)
              DO N = 1,NUMNOD
                NCT = ADSKY(N)-1
                NC = ADSKY(N+1)-ADSKY(N)
                DO K = NCT+1, NCT+NC
                  NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                  NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                  NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                  NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                  NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                  NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                  NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
                  FTHE(N)  = FTHE(N)  + FTHESKY(K)
                ENDDO
              ENDDO
!$OMP END DO
              ! --------------
            ELSEIF(N2D/=0)THEN
!$OMP DO SCHEDULE(guided)
              DO N = 1,NUMNOD
                NCT = ADSKY(N)-1
                NC = ADSKY(N+1)-ADSKY(N)
                DO K = NCT+1, NCT+NC
                  NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                  NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                  NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                  NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                  NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                  NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                  NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
                  FTHE(N)  = FTHE(N)  + FTHESKY(K)
                  MS_2D(N) = MS_2D(N)  + FSKYM(K)
                ENDDO
              ENDDO
!$OMP END DO
              ! --------------
            ELSE
!$OMP DO SCHEDULE(guided)
              DO N = 1,NUMNOD
                NCT = ADSKY(N)-1
                NC = ADSKY(N+1)-ADSKY(N)
                DO K = NCT+1, NCT+NC
                  NODES%A(1,N)   = NODES%A(1,N)   + FSKY(1,K)
                  NODES%A(2,N)   = NODES%A(2,N)   + FSKY(2,K)
                  NODES%A(3,N)   = NODES%A(3,N)   + FSKY(3,K)
                  NODES%AR(1,N)  = NODES%AR(1,N)  + FSKY(4,K)
                  NODES%AR(2,N)  = NODES%AR(2,N)  + FSKY(5,K)
                  NODES%AR(3,N)  = NODES%AR(3,N)  + FSKY(6,K)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKY(7,K)
                  NODES%STIFR(N) = NODES%STIFR(N) + FSKY(8,K)
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
!$OMP DO SCHEDULE(guided)
          DO N = 1,NUMNOD
            NCT = ADSKY(N)-1
            NC = ADSKY(N+1)-ADSKY(N)
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
!$OMP DO SCHEDULE(guided)
          DO N = 1,NUMNOD
            NCT = ADSKY(N)-1
            NC = ADSKY(N+1)-ADSKY(N)
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
!$OMP DO SCHEDULE(guided)
          DO N = 1,NUMNOD
            IL = INOD_PXFEM(N)
            IF(IL > 0) THEN
              NCT = ADSKY_PXFEM(IL) - 1
              NC =  ADSKY_PXFEM(IL+1) - ADSKY_PXFEM(IL)
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

C mass stored in FSKY(1) and NODES%A(1) used for 2D accumulation
        IF(N2D/=0) THEN
!$OMP DO SCHEDULE(guided)
          DO I = 1,NUMNOD
            NODES%MS(I) = NODES%A(1,I)
            NODES%A(1,I) = ZERO
          ENDDO
!$OMP END DO
        ELSEIF(ALE%SUB%IFSUBM==1)THEN
!$OMP DO SCHEDULE(guided)
          DO I = 1,NUMNOD
            NODES%MS(I) = NODES%MS(I) + MSNF(I)
          ENDDO
!$OMP END DO
        ENDIF
C
        IF(IALELAG > 0) THEN
!$OMP DO SCHEDULE(guided)
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
            STIFND(I) = NODES%STIFN(ND)
          ENDDO
!$OMP END SINGLE
        ENDIF
C-----------------------------------------------
C Partie NITSCHE ELEM FORNEQS
C-----------------------------------------------
        IF(NITSCHE /= 0) THEN
!$OMP DO SCHEDULE(guided)
          DO N = 1,NUMNOD !NODFT, NODLT
            NCT = ADSKY(N)-1
            NC = ADSKY(N+1)-ADSKY(N)
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
C Interface part
C-----------------------------------------------
        IF(NISKY>LSKYI)THEN
          WRITE(IOUT,*) ' **ERROR** : MEMORY PROBLEM IN PARITH OPTION'
          WRITE(IOUT,*)
     .      ' PLEASE, INCREASE MULTIMP FOR INTERFACES 7, 10 AND 11'
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
!$OMP DO SCHEDULE(guided)
            DO I=1,NISKY
              J = ISKY(I) ! address for N per contribution
              FSKY_L(J) = FSKYI(I,L) ! FSKYT: contribution sorted by node
            END DO
!$OMP END DO
            call my_barrier()
!$OMP DO SCHEDULE(guided)
            DO I=1,NISKY
              FSKYI(I,L) = FSKY_L(I)
            END DO
!$OMP END DO
          ENDDO
C
          IF(GLOB_THERM%INTHEAT > 0 ) THEN
!$OMP SINGLE
            DO I=1,NISKY
              J = ISKY(I)
              FSKYT(J) = FTHESKYI(I)
            ENDDO
            DO  I=1,NISKY
              FTHESKYI(I) = FSKYT(I)
            ENDDO
!$OMP END SINGLE
            IF(GLOB_THERM%NODADT_THERM ==1 ) THEN
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
C FSKYI and NISKY barrier
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
C   20 Value threshold for call in qsort (20 = NBCC)
              ELSEIF(DIFFADD>=1.AND.DIFFADD<NBCC) THEN
                K2 = K2 + 1
                VIND2(K2) = N
              ELSEIF(DIFFADD>=NBCC) THEN
                K3 = K3 + 1
                VIND3(K3) = N
              ENDIF
            ENDDO
C
C single contribution
C
            IF(GLOB_THERM%INTHEAT == 0 ) THEN
              IF(KDTINT==0)THEN
#include "vectorize.inc"
                DO IJK=1,K1
                  N = VIND1(IJK)
                  K=ADSKYI(N-1)
                  NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                  NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                  NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                ENDDO
              ELSE
#include "vectorize.inc"
                DO IJK=1,K1
                  N = VIND1(IJK)
                  K=ADSKYI(N-1)
                  NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                  NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                  NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                  NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                  NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
                ENDDO
              ENDIF
C + thermal
            ELSE
              IF(GLOB_THERM%NODADT_THERM == 1) THEN
                IF(KDTINT==0)THEN
#include "vectorize.inc"
                  DO IJK=1,K1
                    N = VIND1(IJK)
                    K=ADSKYI(N-1)
                    NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                    NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                    NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                    NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                    FTHE(N) = FTHE(N) + FTHESKYI(K)
                    CONDN(N) = CONDN(N) + CONDNSKYI(K)
                    FTHESKYI(K) = ZERO
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO IJK=1,K1
                    N = VIND1(IJK)
                    K=ADSKYI(N-1)
                    NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                    NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                    NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                    NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                    NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
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
                    NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                    NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                    NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                    NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                    FTHE(N) = FTHE(N) + FTHESKYI(K)
                    FTHESKYI(K) = ZERO
                  ENDDO
                ELSE
#include "vectorize.inc"
                  DO IJK=1,K1
                    N = VIND1(IJK)
                    K=ADSKYI(N-1)
                    NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                    NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                    NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                    NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                    NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
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
C no more than 20 contributions
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
                IF(GLOB_THERM%INTHEAT == 0 ) THEN
                  DO 500 K=JJ1,JJ2-1
                    DO 500 KK=K+1,JJ2
                      DO 500 L=1,NFSKYI
                        IF(FSKYI(KK,L)>FSKYI(K,L))THEN
                          FF = FSKYI(KK,L)
                          FSKYI(KK,L) = FSKYI(K,L)
                          FSKYI(K,L)  = FF
                        ENDIF
  500             CONTINUE
C + thermal
                ELSE
                  IF(GLOB_THERM%NODADT_THERM == 1 ) THEN
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
C       FORCE ASSEMBLY
C-----------------------------------------------

                IF(GLOB_THERM%INTHEAT == 0 ) THEN
                  IF(KDTINT==0)THEN
                    DO K=JJ1,JJ2
                      NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                      NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                      NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                      NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                    ENDDO
                  ELSE
                    DO K=JJ1,JJ2
                      NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                      NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                      NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                      NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                      NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
                    ENDDO
                  ENDIF
C+ thermal analysis
                ELSE
                  IF(GLOB_THERM%NODADT_THERM == 1) THEN
                    IF(KDTINT==0)THEN
                      DO K=JJ1,JJ2
                        NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                        NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                        NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                        NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                        FTHE(N)  = FTHE(N) + FTHESKYI(K)
                        CONDN(N) = CONDN(N)+ CONDNSKYI(K)
                        FTHESKYI(K) = ZERO
                      ENDDO
                    ELSE
                      DO K=JJ1,JJ2
                        NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                        NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                        NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                        NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                        NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
                        FTHE(N)  = FTHE(N) + FTHESKYI(K)
                        CONDN(N) = CONDN(N)+ CONDNSKYI(K)
                        FTHESKYI(K) = ZERO
                      ENDDO
                    ENDIF
                  ELSE
                    IF(KDTINT==0)THEN
                      DO K=JJ1,JJ2
                        NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                        NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                        NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                        NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                        FTHE(N)  = FTHE(N) + FTHESKYI(K)
                        FTHESKYI(K) = ZERO
                      ENDDO
                    ELSE
                      DO K=JJ1,JJ2
                        NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                        NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                        NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                        NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                        NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
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
  800         CONTINUE
            ELSE

            ENDIF
C   FIN NOUVEAUX TRAITEMENTS VECTORIELS
C
C more than 20 contributions
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
              IF(GLOB_THERM%INTHEAT >  0) CALL ASS2SORT(FTHESKYI,JJ1,JJ2,FSKYT,1)
              IF(GLOB_THERM%NODADT_THERM == 1) CALL ASS2SORT(CONDNSKYI,JJ1,JJ2,FSKYT,1)
              IF(INTPLYXFEM>0)
     .        CALL ASS2SORT_PXFEM(PLYSKYI%FSKYI,JJ1,JJ2,FSKYT,5)
C-----------------------------------------------
C       FORCE ASSEMBLY
C-----------------------------------------------
              IF(GLOB_THERM%INTHEAT == 0 ) THEN
                IF(KDTINT==0)THEN
                  DO K=JJ1,JJ2
                    NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                    NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                    NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                    NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                  ENDDO
                ELSE
                  DO K=JJ1,JJ2
                    NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                    NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                    NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                    NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                    NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
                  ENDDO
                ENDIF
C  + thermal
              ELSE
                IF(GLOB_THERM%NODADT_THERM ==1) THEN
                  IF(KDTINT==0)THEN
                    DO K=JJ1,JJ2
                      NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                      NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                      NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                      NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                      FTHE(N) = FTHE(N) + FTHESKYI(K)
                      CONDN(N)= CONDN(N) + CONDNSKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ELSE
                    DO K=JJ1,JJ2
                      NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                      NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                      NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                      NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                      NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
                      FTHE(N) = FTHE(N) + FTHESKYI(K)
                      CONDN(N)= CONDN(N) + CONDNSKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ENDIF
                ELSE
                  IF(KDTINT==0)THEN
                    DO K=JJ1,JJ2
                      NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                      NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                      NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                      NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                      FTHE(N) = FTHE(N) + FTHESKYI(K)
                      FTHESKYI(K) = ZERO
                    ENDDO
                  ELSE
                    DO K=JJ1,JJ2
                      NODES%A(1,N)  = NODES%A(1,N)  + FSKYI(K,1)
                      NODES%A(2,N)  = NODES%A(2,N)  + FSKYI(K,2)
                      NODES%A(3,N)  = NODES%A(3,N)  + FSKYI(K,3)
                      NODES%STIFN(N) = NODES%STIFN(N) + FSKYI(K,4)
                      NODES%VISCN(N) = NODES%VISCN(N) + FSKYI(K,5)
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
C ftheskyi and condnskyi are nor all reset to zero
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

!||====================================================================
!||    asspari2         ../engine/source/assembly/asspar4.F
!||--- called by ------------------------------------------------------
!||    intti1           ../engine/source/interfaces/interf/intti1.F
!||--- uses       -----------------------------------------------------
!||    glob_therm_mod   ../common_source/modules/mat_elem/glob_therm_mod.F90
!||====================================================================
      SUBROUTINE ASSPARI2(
     1  A    ,AR    ,STIFN  ,STIFR  ,MS  ,
     2  IN   ,FSKYI2,I2SIZE ,ADDCNI2,INDSKY,
     3  FTHESKYI2,FTHE,CONDNSKYI2,CONDN,GLOB_THERM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        use glob_therm_mod
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
     .          A(3,*)  ,AR(3,*), STIFN(*), STIFR(*), MS(*), IN(*),
     .          FSKYI2(I2SIZE,*),FTHESKYI2(*),FTHE(*),CONDNSKYI2(*),CONDN(*)
        type (glob_therm_) ,intent(inout)   :: glob_therm
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER K,N,NC,NCT,IDEB,LB,KK,K1,K2
C-----------------------------------------------
C Interface type2
C-----------------------------------------------
        IF (IVECTOR==1) THEN
C
        ELSE
C
C scalaire
          IF(GLOB_THERM%INTHEAT == 0 ) THEN
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
