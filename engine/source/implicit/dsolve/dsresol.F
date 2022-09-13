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
Cf51e6 Nouvelle routine
Chd|====================================================================
Chd|  DSRESOL                       source/implicit/dsolve/dsresol.F
Chd|-- called by -----------
Chd|        IMP_DSOLV                     source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        DSEIG                         source/implicit/dsolve/dseig.F
Chd|        DSMSHIFT                      source/implicit/dsolve/dsdyn.F
Chd|        SPMD_DS_IRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_ISEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_RRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_RSEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DSRESOL(GRAPHE, IOPT  , NSCM  , NDDL  , B     ,
     .                   NM    , MAXITR, TOL   , CUTFRQ, NBLF  ,
     .                   IPRI  , NVD   , FREQ2 , NVAL  , SHIFTI,
     .                   TITRE , LTITR1 , IPMESS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IOPT, NSCM, NDDL, NM, MAXITR, NBLF, IPRI, NVD, NVAL,
     .        LTITR1, IPMESS
      my_real
     .        B(NSCM,*), TOL, CUTFRQ, FREQ2(*), SHIFTI
      TYPE(PRGRAPH) :: GRAPHE(*)
      CHARACTER TITRE*(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), ITAGI(NDDL), NMOD, J, JJ, N, IADM, 
     .        IFILE, NDF, NDM, IM, LEN, IREC, IKC, IR, NR, IAD1,
     .        K, KK, IMC, IM0, ISTOP, NEV, NCV, MAXN, MAXNEV, MAXNCV, 
     .        LDV, ISHFTS, MODE, INFO, IMDIAG, NRHS, INFFAC, INFRES,
     .        NDEPL, NMDYN, NN, NBLF0, NUV, II, IDBL, IDF, NIB, NVD0,
     .        INOPRI, LFACW
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPIV
      my_real
     .        SHIFT, TSHIFT, PULS, FRQ, FR1, FR2,
     .        TSHIFT0, RLFACW
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KL, ML, S, FACW
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: F, VECT, V, RIGI, MASS,
     .                                        FREQ, RIGIS, VL
      CHARACTER WHICH*2
      INTEGER NDFMAX, IPARAM(2,NSPMD-1), NDFP, NDMP, LENP, IRQTAG,
     .        MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: PDDLS
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: BL
      DATA MSGOFF  /200000/ 
      DATA MSGOFF2 /300000/
      DATA MSGOFF3 /400000/
      DATA MSGOFF4 /500000/
C
      DO I=1,NDDL
         ITAG(I)=0
         ITAGI(I)=0
      ENDDO
      IF (IMACH/=3) THEN
         NMOD=0
         DO I=1,NSPROC
            DO J=1,GRAPHE(I)%DGRAPH(1)%NDDL_F
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               ITAG(JJ)=1
            ENDDO
            NMOD=NMOD+GRAPHE(I)%DGRAPH(1)%NMDYN
         ENDDO
         N=0
         DO I=1,NDDL
            N=N+ITAG(I)
            IF (N>0.AND.ITAG(I)/=0) ITAGI(N)=I
            ITAG(I)=N
         ENDDO
         IADM=N
         NN=N
         N=N+NMOD
         IF (N==0) RETURN
C
         IF (IPMESS/=0) THEN
            WRITE(IOUT,'(A)') TITRE(1:LTITR1)
            WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         ENDIF
C
         ALLOCATE(RIGI(N,N))
         DO I=1,N
            DO J=1,N
               RIGI(I,J)=ZERO
            ENDDO
         ENDDO
         IF (IOPT==1) THEN
            ALLOCATE(F(NSCM,N))
            DO I=1,NSCM
               DO J=1,N
                  JJ=ITAGI(J)
                  F(I,J)=B(I,JJ)
               ENDDO
            ENDDO
         ENDIF
         IF (IOPT==2) THEN
            ALLOCATE(MASS(N,N))
            DO I=1,N
               DO J=1,N
                  MASS(I,J)=ZERO
               ENDDO
            ENDDO
         ENDIF
      ELSEIF (ISPMD==0) THEN
C SPMD - Tags :
C Requetes entieres : parametres : MSGOFF + IPROC
C                     liste ddls : MSGOFF + (NSPMD-1) + IPROC
         NDFMAX=0
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + I
            CALL SPMD_DS_IRECV(IPARAM(1,I), 2, IRQTAG, I+1)
            NDFMAX=MAX(NDFMAX,IPARAM(1,I))
         ENDDO
C Tous les pros ont finis leur condensation : on affiche le titre
C de la resolution
         IF (IPMESS/=0) THEN
            WRITE(IOUT,'(A)') TITRE(1:LTITR1)
            WRITE(ISTDO,*)
            WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         ENDIF
C
         ALLOCATE(PDDLS(NDFMAX,NSPMD-1))
         DO I=1,GRAPHE(1)%DGRAPH(1)%NDDL_F
            II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
            II=GRAPHE(1)%LSDDL_GLOB(II)
            ITAG(II)=1
         ENDDO
         NMOD=GRAPHE(1)%DGRAPH(1)%NMDYN
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + (NSPMD-1) + I
            CALL SPMD_DS_IRECV(PDDLS(1,I), IPARAM(1,I), IRQTAG, I+1)
            NDF=IPARAM(1,I)
            DO J=1,NDF
               JJ=PDDLS(J,I)
               ITAG(JJ)=1
            ENDDO
            NMOD=NMOD+IPARAM(2,I)
         ENDDO
         N=0
         DO I=1,NDDL
            N=N+ITAG(I)
            IF (N>0.AND.ITAG(I)/=0) ITAGI(N)=I
            ITAG(I)=N
         ENDDO
         IADM=N
         NN=N
         N=N+NMOD
         CALL SPMD_IBCAST(N, N, 1, 1, 0, 2)
         IF (N==0) RETURN
C
         ALLOCATE(RIGI(N,N))
         DO I=1,N
            DO J=1,N
               RIGI(I,J)=ZERO
            ENDDO
         ENDDO
         IF (IOPT==1) THEN
            ALLOCATE(F(NSCM,N), BL(NDFMAX*NSCM))
            DO I=1,NSCM
               DO J=1,N
                  F(I,J)=ZERO
               ENDDO
            ENDDO
            DO I=1,GRAPHE(1)%DGRAPH(1)%NDDL_F
               II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
               J=GRAPHE(1)%LSDDL_GLOB(II)
               JJ=ITAG(J)
               DO K=1,NSCM
                  F(K,JJ)=B(K,II)
               ENDDO
            ENDDO
C
            DO I=1,NSPMD-1
               IRQTAG=MSGOFF2 + (NSPMD-1) + I
               LEN=NSCM*IPARAM(1,I)
               CALL SPMD_DS_RRECV(BL, LEN, IRQTAG, I+1)
               IAD1=0
               DO J=1,NSCM
                  DO K=1,IPARAM(1,I)
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     F(J,KK)=F(J,KK)+BL(IAD1+K)
                  ENDDO
                  IAD1=IAD1+IPARAM(1,I)
               ENDDO
            ENDDO
C
            DEALLOCATE(BL)
         ENDIF
         IF (IOPT==2) THEN
            ALLOCATE(MASS(N,N))
            DO I=1,N
               DO J=1,N
                  MASS(I,J)=ZERO
               ENDDO
            ENDDO
         ENDIF
      ELSE
         IPARAM(1,1)=GRAPHE(1)%DGRAPH(1)%NDDL_F
         IPARAM(2,1)=GRAPHE(1)%DGRAPH(1)%NMDYN
         IRQTAG=MSGOFF + ISPMD
         CALL SPMD_DS_ISEND(IPARAM(1,1), 2, IRQTAG, 1)
         ALLOCATE(PDDLS(IPARAM(1,1),1))
         DO I=1,IPARAM(1,1)
            II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
            II=GRAPHE(1)%LSDDL_GLOB(II)
            PDDLS(I,1)=II
         ENDDO
         IRQTAG=MSGOFF + (NSPMD-1) + ISPMD
         CALL SPMD_DS_ISEND(PDDLS, IPARAM(1,1), IRQTAG, 1)
C
         CALL SPMD_IBCAST(N, N, 1, 1, 0, 2)
         IF (N==0) RETURN
C
         IF (IOPT==1) THEN
            ALLOCATE(BL(IPARAM(1,1)*NSCM))
            IAD1=0
            DO I=1,NSCM
               DO J=1,IPARAM(1,1)
                  JJ=GRAPHE(1)%DGRAPH(1)%DDLS(J)
                  BL(IAD1+J)=B(I,JJ)
               ENDDO
               IAD1=IAD1+IPARAM(1,1)
            ENDDO
            IRQTAG=MSGOFF2 + (NSPMD-1) + ISPMD
            LEN=NSCM*IPARAM(1,1)
            CALL SPMD_DS_RSEND(BL, LEN, IRQTAG, 1)
            DEALLOCATE(BL)
         ENDIF
      ENDIF
C
      IF (IMACH/=3) THEN
         IM=0
         DO I=1,NSPROC
            IFILE=GRAPHE(I)%IFILE
            NDF=GRAPHE(I)%DGRAPH(1)%NDDL_F
            NDM=GRAPHE(I)%DGRAPH(1)%NMDYN
            LEN=(NDF+NDM)*(NDF+NDM+1)/2
C Assemblage de la rigidite
            ALLOCATE(KL(LEN))
            IREC=GRAPHE(I)%DGRAPH(1)%KREC
            IKC=0
            DO IR=1,LEN/10
               READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
               IKC=IKC+10
               IREC=IREC+1
            ENDDO
            NR=LEN-(LEN/10)*10
            IF (NR>0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C            
            DO J=1,NDF
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                  RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
               ENDDO
               RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
            ENDDO
            IM0=IM
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  RIGI(NN+IM,KK)=RIGI(NN+IM,KK)+KL(IAD1+K)
                  RIGI(KK,NN+IM)=RIGI(KK,NN+IM)+KL(IAD1+K)
               ENDDO
               DO K=1,IM-IM0-1
                  RIGI(NN+IM,NN+IM0+K)=
     .                       RIGI(NN+IM,NN+IM0+K)+KL(IAD1+NDF+K)
                  RIGI(NN+IM0+K,NN+IM)=
     .                       RIGI(NN+IM0+K,NN+IM)+KL(IAD1+NDF+K)
               ENDDO
               RIGI(NN+IM,NN+IM)=RIGI(NN+IM,NN+IM)+KL(IAD1+NDF+IM-IM0)
            ENDDO
            DEALLOCATE(KL)       
C Assemblage des seconds membres
            IF (IOPT==1) THEN
               DO J=1,NSCM
                  DO K=1,NDF
                     KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                     KK=ITAG(KK)
                     F(J,KK)=F(J,KK)+GRAPHE(I)%DGRAPH(1)%B(J,K)
                  ENDDO
               ENDDO
               CYCLE
            ENDIF
C Assemblage de la masse
            ALLOCATE(ML(LEN))
            IREC=GRAPHE(I)%DGRAPH(1)%MREC
            IMC=0
            DO IR=1,LEN/10
               READ(IFILE,REC=IREC) (ML(IMC+J),J=1,10)
               IMC=IMC+10
               IREC=IREC+1
            ENDDO
            NR=LEN-(LEN/10)*10
            IF (NR>0) READ(IFILE,REC=IREC) (ML(IMC+J),J=1,NR)
C            
            DO J=1,NDF
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  MASS(JJ,KK)=MASS(JJ,KK)+ML(IAD1+K)
                  MASS(KK,JJ)=MASS(KK,JJ)+ML(IAD1+K)
               ENDDO
               MASS(JJ,JJ)=MASS(JJ,JJ)+ML(IAD1+J)
            ENDDO
            IM=IM0
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  MASS(NN+IM,KK)=MASS(NN+IM,KK)+ML(IAD1+K)
                  MASS(KK,NN+IM)=MASS(KK,NN+IM)+ML(IAD1+K)
               ENDDO
               DO K=1,IM-IM0-1
                  MASS(NN+IM,NN+IM0+K)=
     .                         MASS(NN+IM,NN+IM0+K)+ML(IAD1+NDF+K)
                  MASS(NN+IM0+K,NN+IM)=
     .                         MASS(NN+IM0+K,NN+IM)+ML(IAD1+NDF+K)
               ENDDO
               MASS(NN+IM,NN+IM)=MASS(NN+IM,NN+IM)+ML(IAD1+NDF+IM-IM0)
            ENDDO
            DEALLOCATE(ML)       
         ENDDO
      ELSE
C SPMD - Tags :
C Requetes reelles  : matrice rigidite  : MSGOFF2 + IPROC
C                     forces            : MSGOFF2 + (NSPMD-1) + IPROC
C                     forces condensees : MSGOFF2 + 2*(NSPMD-1) + IPROC
C                     matrice masse     : MSGOFF2 + 3*(NSPMD-1) + IPROC
         IFILE=GRAPHE(1)%IFILE
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         NDM=GRAPHE(1)%DGRAPH(1)%NMDYN
         LEN=(NDF+NDM)*(NDF+NDM+1)/2
C Assemblage de la rigidite
         ALLOCATE(KL(LEN))
         IREC=GRAPHE(1)%DGRAPH(1)%KREC
         IKC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
            IKC=IKC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR>0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C            
         IF (ISPMD==0) THEN
C Processeur zero : recoit et assemble       
            IM=0
            DO J=1,NDF
               JJ=GRAPHE(1)%DGRAPH(1)%DDLS(J)
               JJ=GRAPHE(1)%LSDDL_GLOB(JJ)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                  RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
               ENDDO
               RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
            ENDDO
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  RIGI(NN+IM,KK)=RIGI(NN+IM,KK)+KL(IAD1+K)
                  RIGI(KK,NN+IM)=RIGI(KK,NN+IM)+KL(IAD1+K)
               ENDDO
               DO K=1,IM-1
                  RIGI(NN+IM,NN+K)=
     .                         RIGI(NN+IM,NN+K)+KL(IAD1+NDF+K)
                  RIGI(NN+K,NN+IM)=
     .                         RIGI(NN+K,NN+IM)+KL(IAD1+NDF+K)
               ENDDO
               RIGI(NN+IM,NN+IM)=RIGI(NN+IM,NN+IM)+KL(IAD1+NDF+IM)
            ENDDO
            DEALLOCATE(KL)
C
            DO I=1,NSPMD-1
               NDFP=IPARAM(1,I)
               NDMP=IPARAM(2,I)
               LENP=(NDFP+NDMP)*(NDFP+NDMP+1)/2
               ALLOCATE(KL(LENP))
               IRQTAG=MSGOFF2 + I
               CALL SPMD_DS_RRECV(KL, LENP, IRQTAG, I+1)
               DO J=1,NDFP
                  JJ=PDDLS(J,I)
                  JJ=ITAG(JJ)
                  IAD1=(J-1)*J/2
                  DO K=1,J-1
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                     RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
                  ENDDO
                  RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
               ENDDO
               IM0=IM
               DO J=1,NDMP
                  IM=IM+1
                  IAD1=(NDFP+J-1)*(NDFP+J)/2
                  DO K=1,NDFP
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     RIGI(NN+IM,KK)=RIGI(NN+IM,KK)+KL(IAD1+K)
                     RIGI(KK,NN+IM)=RIGI(KK,NN+IM)+KL(IAD1+K)
                  ENDDO
                  DO K=1,IM-IM0-1
                     RIGI(NN+IM,NN+IM0+K)=
     .                        RIGI(NN+IM,NN+IM0+K)+KL(IAD1+NDFP+K)
                     RIGI(NN+IM0+K,NN+IM)=
     .                        RIGI(NN+IM0+K,NN+IM)+KL(IAD1+NDFP+K)
                  ENDDO
                  RIGI(NN+IM,NN+IM)=
     .               RIGI(NN+IM,NN+IM)+KL(IAD1+NDFP+IM-IM0)
               ENDDO
               DEALLOCATE(KL)
            ENDDO
C
         ELSE
C Autres processeurs : envoient     
            IRQTAG=MSGOFF2 + ISPMD
            CALL SPMD_DS_RSEND(KL, LEN, IRQTAG, 1)
            DEALLOCATE(KL)
         ENDIF
C Assemblage des seconds membres
         IF (IOPT==1) THEN
            IF (ISPMD==0) THEN
C Processeur zero : recoit et assemble       
               DO J=1,NSCM
                  DO K=1,NDF
                     KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                     KK=GRAPHE(1)%LSDDL_GLOB(KK)
                     KK=ITAG(KK)
                     F(J,KK)=F(J,KK)+GRAPHE(1)%DGRAPH(1)%B(J,K)
                  ENDDO
               ENDDO
C
               ALLOCATE(BL(NSCM*NDFMAX))
               DO I=1,NSPMD-1
                  IRQTAG=MSGOFF2 + 2*(NSPMD-1) + I
                  LENP=IPARAM(1,I)*NSCM
                  CALL SPMD_DS_RRECV(BL, LENP, IRQTAG, I+1)
                  IAD1=0
                  DO J=1,NSCM
                     DO K=1,IPARAM(1,I)
                        KK=PDDLS(K,I)
                        KK=ITAG(KK)
                        F(J,KK)=F(J,KK)+BL(IAD1+K)
                     ENDDO
                     IAD1=IAD1+IPARAM(1,I)
                  ENDDO
               ENDDO
               DEALLOCATE(BL)
            ELSE
C Autres processeurs : envoient     
               ALLOCATE(BL(NSCM*IPARAM(1,1)))
               IAD1=0
               DO I=1,NSCM
                  DO J=1,IPARAM(1,1)
                     BL(IAD1+J)=GRAPHE(1)%DGRAPH(1)%B(I,J)
                  ENDDO
                  IAD1=IAD1+IPARAM(1,1)
               ENDDO
               IRQTAG=MSGOFF2 + 2*(NSPMD-1) + ISPMD
               CALL SPMD_DS_RSEND(BL, NSCM*IPARAM(1,1), IRQTAG, 1)
               DEALLOCATE(BL)
            ENDIF
            GOTO 100
         ENDIF
C Assemblage de la masse
         ALLOCATE(ML(LEN))
         IREC=GRAPHE(1)%DGRAPH(1)%MREC
         IMC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (ML(IMC+J),J=1,10)
            IMC=IMC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR>0) READ(IFILE,REC=IREC) (ML(IMC+J),J=1,NR)
C            
         IF (ISPMD==0) THEN
C Processeur zero : recoit et assemble       
            IM=0
            DO J=1,NDF
               JJ=GRAPHE(1)%DGRAPH(1)%DDLS(J)
               JJ=GRAPHE(1)%LSDDL_GLOB(JJ)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  MASS(JJ,KK)=MASS(JJ,KK)+ML(IAD1+K)
                  MASS(KK,JJ)=MASS(KK,JJ)+ML(IAD1+K)
               ENDDO
               MASS(JJ,JJ)=MASS(JJ,JJ)+ML(IAD1+J)
            ENDDO
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  MASS(NN+IM,KK)=MASS(NN+IM,KK)+ML(IAD1+K)
                  MASS(KK,NN+IM)=MASS(KK,NN+IM)+ML(IAD1+K)
               ENDDO
               DO K=1,IM-1
                  MASS(NN+IM,NN+K)=
     .                          MASS(NN+IM,NN+K)+ML(IAD1+NDF+K)
                  MASS(NN+K,NN+IM)=
     .                          MASS(NN+K,NN+IM)+ML(IAD1+NDF+K)
               ENDDO
               MASS(NN+IM,NN+IM)=MASS(NN+IM,NN+IM)+ML(IAD1+NDF+IM)
            ENDDO
            DEALLOCATE(ML)       
C
            DO I=1,NSPMD-1
               NDFP=IPARAM(1,I)
               NDMP=IPARAM(2,I)
               LENP=(NDFP+NDMP)*(NDFP+NDMP+1)/2
               ALLOCATE(ML(LENP))
               IRQTAG=MSGOFF2 + 3*(NSPMD-1) + I
               CALL SPMD_DS_RRECV(ML, LENP, IRQTAG, I+1)
               DO J=1,NDFP
                  JJ=PDDLS(J,I)
                  JJ=ITAG(JJ)
                  IAD1=(J-1)*J/2
                  DO K=1,J-1
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     MASS(JJ,KK)=MASS(JJ,KK)+ML(IAD1+K)
                     MASS(KK,JJ)=MASS(KK,JJ)+ML(IAD1+K)
                  ENDDO
                  MASS(JJ,JJ)=MASS(JJ,JJ)+ML(IAD1+J)
               ENDDO
               IM0=IM
               DO J=1,NDMP
                  IM=IM+1
                  IAD1=(NDFP+J-1)*(NDFP+J)/2
                  DO K=1,NDFP
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     MASS(NN+IM,KK)=MASS(NN+IM,KK)+ML(IAD1+K)
                     MASS(KK,NN+IM)=MASS(KK,NN+IM)+ML(IAD1+K)
                  ENDDO
                  DO K=1,IM-IM0-1
                     MASS(NN+IM,NN+IM0+K)=
     .                        MASS(NN+IM,NN+IM0+K)+ML(IAD1+NDFP+K)
                     MASS(NN+IM0+K,NN+IM)=
     .                        MASS(NN+IM0+K,NN+IM)+ML(IAD1+NDFP+K)
                  ENDDO
                  MASS(NN+IM,NN+IM)=
     .               MASS(NN+IM,NN+IM)+ML(IAD1+NDFP+IM-IM0)
               ENDDO
               DEALLOCATE(ML)
            ENDDO
         ELSE
C Autres processeurs : envoient     
            IRQTAG=MSGOFF2 + 3*(NSPMD-1) + ISPMD
            CALL SPMD_DS_RSEND(ML, LEN, IRQTAG, 1)
            DEALLOCATE(ML)
         ENDIF
C
 100     CONTINUE
C
      ENDIF           
C     
      IF (IMACH/=3.OR.ISPMD==0) THEN
         IF (IOPT==1) THEN
C Resolution systeme lineaire pour NSCM seconds membres
            ALLOCATE(V(N,NSCM), S(N), IPIV(N))
C Block size query
            LFACW=-1
            CALL DSYTRF('U', N, RIGI, N, IPIV, RLFACW, LFACW, INFFAC)
            LFACW=NINT(RLFACW)
            ALLOCATE(FACW(LFACW))
C Factorization
            CALL DSYTRF('U', N, RIGI, N, IPIV, FACW, LFACW, INFFAC)
            DO I=1,NSCM
               DO J=1,N
                  S(J)=F(I,J)
               ENDDO
               NRHS=1
               CALL DSYTRS('U', N, NRHS, RIGI, N, IPIV, S, N, INFRES)
               IF (INFRES/=0) THEN
                   CALL ANCMSG(MSGID=62,ANMODE=ANINFO)
                   CALL ARRET(2)
               ENDIF
               DO J=1,N
                  V(J,I)=S(J)
               ENDDO
            ENDDO
            DEALLOCATE(S, IPIV, FACW)
            NDEPL=NSCM
C               
         ELSEIF (IOPT==2) THEN
C Resolution probleme aux valeurs propres
            MAXNCV=NM*NSVMAXT
            ALLOCATE(VECT(N,MAXNCV), V(N,NSVMAXT), RIGIS(N,N),
     .               FREQ(MAXNCV,2))
            VECT = 0
            FREQ = 0
            SHIFT=SHIFTI
            TSHIFT=SHIFT
            WRITE(IOUT,FMT='(A15,1PG11.4)') ' INITIAL SHIFT ',SHIFT
            ISTOP=0
            NVD=0
            IMDIAG=0
C
            NIB=0
            NBLF=MIN(NBLF,NSVMAXT)
            NBLF0=NBLF
            NBLF=MIN(NBLF0,N-2)
C
            DO WHILE (ISTOP==0)
               NVD0=NVD
               INOPRI=0
               TSHIFT0=TSHIFT
               CALL DSMSHIFT(RIGIS, RIGI, MASS, TSHIFT, IMDIAG,
     .                       N    , N   )
               NEV=MIN(NBLF+1,NVAL-NVD)
               NCV=MIN(NM*NEV,N)
               MAXN=N
               MAXNEV=NEV
               LDV=MAXN
               WHICH='LA'
               ISHFTS=1
               MODE=3
               INFO=0
C
               CALL DSEIG(N,      NEV,    NCV,    WHICH,  INFO,
     .                    MAXN,   MAXNEV, MAXNCV, LDV,    ISHFTS,
     .                    MAXITR, MODE,   TOL,    RIGIS,  MASS, 
     .                    FREQ,   VECT,   IPRI,   IMDIAG, TSHIFT,
     .                    N     )
C
               NUV=NEV-1
               IF (NEV==NVAL-NVD) NUV=NEV
C
               DO I=1,NUV
                  PULS=MAX(ZERO,FREQ(I,1))
                  FRQ=SQRT(PULS)/TWO/PI
                  IF (FRQ<=CUTFRQ.AND.NVD<NSVMAXT.
     .                AND.NVD<NVAL) THEN
                     NVD=NVD+1
                     FREQ2(NVD)=FRQ
                     DO II=1,N
                        V(II,NVD)=VECT(II,I)
                     ENDDO
                     IF (NVD==NSVMAXT) THEN
*                        WRITE(ISTDO,'(A84,I5)') 
*     .    ' ** WARNING : MAXIMAL DYNAMIC MODES NUMBER REACHED FOR'//
*     .    ' CONDENSED PROBLEM - NSVMAXT: ',NSVMAXT
*                        WRITE(IOUT,'(A86,I5)') 
*     .    ' ** WARNING : MAXIMAL DYNAMIC MODES NUMBER REACHED FOR'//
*     .    ' CONDENSED PROBLEM - NSVMAXT: ',NSVMAXT
                        ISTOP=1
                     ENDIF
                  ELSE
                     ISTOP=1
                  ENDIF
               ENDDO
C
               IF (NVD==NVAL) ISTOP=1
C
               IF (ISTOP==0) THEN
                  FR1=FREQ(NEV,1)
                  IDBL=0
                  IDF=2
                  DO WHILE (IDBL==0.AND.IDF<=NEV)
                     FR2=FREQ(NEV-IDF+1,1)
                     IF (ABS((FR2-FR1)/FR1)<=EM01) THEN
                        IDF=IDF+1
                        NVD=NVD-1
                     ELSE
                        IDBL=1
                     ENDIF
                  ENDDO
                  IF (IDF==NEV+1) THEN
                     NBLF=2*NBLF
                     SHIFT=ZERO
                     INOPRI=1
                  ELSE
                     NBLF=MIN(NBLF0,N-2)
                     FR1=FREQ(NEV-IDF+2,1)
                     SHIFT=FR2+(FR1-FR2)*ZEP9-TSHIFT
                  ENDIF
                  TSHIFT=TSHIFT+SHIFT
               ENDIF
C
               IF (INOPRI==0) THEN
                  NIB=NIB+1
                  WRITE(IOUT,*)
                  WRITE(IOUT,FMT='(A7,I10,A7,1PG11.4,A23,I10)') 
     .                     ' BLOCK ',NIB,' SHIFT ',TSHIFT0,
     .                     ' NUMBER OF FREQUENCIES ',(NVD-NVD0)
                  WRITE(IOUT,FMT='(A13)') ' FREQUENCIES:'
                  WRITE(IOUT,FMT='(A39)')
     . '         NUMBER  EIGENVALUE   FREQUENCY'
                  DO I=NVD0+1,NVD
                     WRITE(IOUT,FMT='(5X,I10,1X,1PG11.4,1X,1PG11.4)')
     .                        I,(TWO*PI*FREQ2(I))**2,FREQ2(I)
                  ENDDO
               ENDIF
C
            ENDDO
            DEALLOCATE(VECT, RIGIS, FREQ)
            NDEPL=NVD
            WRITE(IOUT,*)
         ENDIF
      ENDIF
C
C Redescente des deplacements
C
      IF (IMACH/=3) THEN
         IM0=0
         DO I=1,NSPROC
            NDF=GRAPHE(I)%DGRAPH(1)%NDDL_F
            NMDYN=GRAPHE(I)%DGRAPH(1)%NMDYN
            DO J=1,NDEPL
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  GRAPHE(I)%DGRAPH(1)%D(J,K)=V(KK,J)
               ENDDO
               IM=IM0
               DO K=1,NMDYN
                  IM=IM+1
                  GRAPHE(I)%DGRAPH(1)%DM(J,K)=V(IADM+IM,J)
               ENDDO
            ENDDO
            IM0=IM0+NMDYN
         ENDDO
      ELSEIF (ISPMD==0) THEN
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         NMDYN=GRAPHE(1)%DGRAPH(1)%NMDYN
         DO J=1,NDEPL
            DO K=1,NDF
               KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
               KK=GRAPHE(1)%LSDDL_GLOB(KK)
               KK=ITAG(KK)
               GRAPHE(1)%DGRAPH(1)%D(J,K)=V(KK,J)
            ENDDO
            IM=0
            DO K=1,NMDYN
               IM=IM+1
               GRAPHE(1)%DGRAPH(1)%DM(J,K)=V(IADM+IM,J)
            ENDDO
         ENDDO
         IADM=IADM+NMDYN
C Envoie les deplacements aux autres processeurs
C Tags : 
C Requetes entieres : parametres   : MSGOFF3 + IPROC
C Requetes reelles  : deplacements : MSGOFF4 + IPROC
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF3 + I
            CALL SPMD_DS_ISEND(NDEPL, 1, IRQTAG, I+1)
C
            NDFP=IPARAM(1,I)
            NDMP=IPARAM(2,I)
            ALLOCATE(VL(NDFP+NDMP,NDEPL))
            DO J=1,NDEPL
               DO K=1,NDFP
                  KK=PDDLS(K,I)
                  KK=ITAG(KK)
                  VL(K,J)=V(KK,J)
               ENDDO
               DO K=1,NDMP
                  VL(NDFP+K,J)=V(IADM+K,J)
               ENDDO
            ENDDO
            IADM=IADM+NDMP
C
            LEN=NDEPL*(NDFP+NDMP)
            IRQTAG=MSGOFF4 + I
            CALL SPMD_DS_RSEND(VL, LEN, IRQTAG, I+1)
            DEALLOCATE(VL)
         ENDDO
         DEALLOCATE(PDDLS)
      ELSE
C Recoit les deplacements du processeur zero
         IRQTAG=MSGOFF3 + ISPMD
         CALL SPMD_DS_IRECV(NDEPL, 1, IRQTAG, 1)
         NVD=NDEPL
C
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         NMDYN=GRAPHE(1)%DGRAPH(1)%NMDYN
         ALLOCATE(VL(NDF+NMDYN,NDEPL))
         IRQTAG=MSGOFF4 + ISPMD
         LEN=NDEPL*(NDF+NMDYN)
         CALL SPMD_DS_RRECV(VL, LEN, IRQTAG, 1)
C
         DO J=1,NDEPL
            DO K=1,NDF
               GRAPHE(1)%DGRAPH(1)%D(J,K)=VL(K,J)
            ENDDO
            DO K=1,NMDYN
               GRAPHE(1)%DGRAPH(1)%DM(J,K)=VL(NDF+K,J)
            ENDDO
         ENDDO
         DEALLOCATE(VL, PDDLS)
      ENDIF
C
      IF (IMACH/=3.OR.ISPMD==0) THEN
         DEALLOCATE(V)
C
         IF (IOPT==1) DEALLOCATE(F)
         IF (IOPT==2) DEALLOCATE(MASS)
         DEALLOCATE(RIGI)
      ENDIF
C
      RETURN
      END
Chd|====================================================================
Chd|  DSRESOL_ITER                  source/implicit/dsolve/dsresol.F
Chd|-- called by -----------
Chd|        IMP_DSOLV_ITER                source/implicit/imp_dsolv.F   
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ARRET                         source/system/arret.F         
Chd|        SPMD_DS_IRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_ISEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_RRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_RSEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE DSRESOL_ITER(GRAPHE, IT  , NSCM, NDDL , B    ,
     .                        RIGI  , IPIV, NK  , TITRE, LTITR1,
     .                        IPMESS)       
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IT, NSCM, NDDL, IPIV(*), NK, LTITR1, IPMESS
      my_real
     .        B(NSCM,*), RIGI(NK,*)
      CHARACTER TITRE*(*)
      TYPE(PRGRAPH) :: GRAPHE(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), ITAGI(NDDL), J, JJ, N, NDFMAX, IRQTAG,
     .        IPARAM(2,NSPMD-1), II, LEN, IAD1, NDF, IFILE, IREC, 
     .        IKC, NR, NDFP, LENP, LFACW, INFFAC, NRHS, INFRES, NDEPL,
     .        K, KK, IR
      INTEGER MSGOFF, MSGOFF2, MSGOFF3, MSGOFF4
      my_real
     .        RLFACW
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: PDDLS
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KL, BL, S, FACW
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: F, V, VL
C
      DATA MSGOFF  /200000/ 
      DATA MSGOFF2 /300000/
      DATA MSGOFF3 /400000/
      DATA MSGOFF4 /500000/
C
      DO I=1,NDDL
         ITAG(I)=0
         ITAGI(I)=0
      ENDDO
      IF (IMACH/=3) THEN
         DO I=1,NSPROC
            DO J=1,GRAPHE(I)%DGRAPH(1)%NDDL_F
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               ITAG(JJ)=1
            ENDDO
         ENDDO
         N=0
         DO I=1,NDDL
            N=N+ITAG(I)
            IF (N>0.AND.ITAG(I)/=0) ITAGI(N)=I
            ITAG(I)=N
         ENDDO
         IF (N==0) RETURN
C
         IF (IPMESS/=0) THEN
            WRITE(IOUT,'(A)') TITRE(1:LTITR1)
            WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         ENDIF
C
         IF (IDSC==1) THEN
            DO I=1,N
               DO J=1,N
                  RIGI(I,J)=ZERO
               ENDDO
            ENDDO
         ENDIF
         ALLOCATE(F(NSCM,N))
         DO I=1,NSCM
            DO J=1,N
               JJ=ITAGI(J)
               F(I,J)=B(I,JJ)
            ENDDO
         ENDDO
      ELSEIF (ISPMD==0) THEN
C SPMD - Tags :
C Requetes entieres : parametres : MSGOFF + IPROC
C                     liste ddls : MSGOFF + (NSPMD-1) + IPROC
         NDFMAX=0
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + I
            CALL SPMD_DS_IRECV(IPARAM(1,I), 2, IRQTAG, I+1)
            NDFMAX=MAX(NDFMAX,IPARAM(1,I))
         ENDDO
C
         ALLOCATE(PDDLS(NDFMAX,NSPMD-1))
         DO I=1,GRAPHE(1)%DGRAPH(1)%NDDL_F
            II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
            II=GRAPHE(1)%LSDDL_GLOB(II)
            ITAG(II)=1
         ENDDO
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + (NSPMD-1) + I
            CALL SPMD_DS_IRECV(PDDLS(1,I), IPARAM(1,I), IRQTAG, I+1)
            NDF=IPARAM(1,I)
            DO J=1,NDF
               JJ=PDDLS(J,I)
               ITAG(JJ)=1
            ENDDO
         ENDDO
         N=0
         DO I=1,NDDL
            N=N+ITAG(I)
            IF (N>0.AND.ITAG(I)/=0) ITAGI(N)=I
            ITAG(I)=N
         ENDDO
         CALL SPMD_IBCAST(N, N, 1, 1, 0, 2)
         IF (N==0) RETURN
C Tous les pros ont finis leur condensation : on affiche le titre
C de la resolution
         IF (IPMESS/=0) THEN
            WRITE(IOUT,'(A)') TITRE(1:LTITR1)
            WRITE(ISTDO,*)
            WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         ENDIF
C
         IF (IDSC==1) THEN
            DO I=1,N
               DO J=1,N
                  RIGI(I,J)=ZERO
               ENDDO
            ENDDO
         ENDIF
C
         ALLOCATE(F(NSCM,N), BL(NDFMAX*NSCM))
         DO I=1,GRAPHE(1)%DGRAPH(1)%NDDL_F
            II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
            J=GRAPHE(1)%LSDDL_GLOB(II)
            JJ=ITAG(J)
            DO K=1,NSCM
               F(K,JJ)=B(K,II)
            ENDDO
         ENDDO
C
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF2 + (NSPMD-1) + I
            LEN=NSCM*IPARAM(1,I)
            CALL SPMD_DS_RRECV(BL, LEN, IRQTAG, I+1)
            IAD1=0
            DO J=1,NSCM
               DO K=1,IPARAM(1,I)
                  KK=PDDLS(K,I)
                  KK=ITAG(KK)
                  F(J,KK)=F(J,KK)+BL(IAD1+K)
               ENDDO
               IAD1=IAD1+IPARAM(1,I)
            ENDDO
         ENDDO
         DEALLOCATE(BL)
      ELSE
         IPARAM(1,1)=GRAPHE(1)%DGRAPH(1)%NDDL_F
         IRQTAG=MSGOFF + ISPMD
         CALL SPMD_DS_ISEND(IPARAM(1,1), 2, IRQTAG, 1)
         ALLOCATE(PDDLS(IPARAM(1,1),1))
         DO I=1,IPARAM(1,1)
            II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
            II=GRAPHE(1)%LSDDL_GLOB(II)
            PDDLS(I,1)=II
         ENDDO
         IRQTAG=MSGOFF + (NSPMD-1) + ISPMD
         CALL SPMD_DS_ISEND(PDDLS, IPARAM(1,1), IRQTAG, 1)
C
         CALL SPMD_IBCAST(N, N, 1, 1, 0, 2)
         IF (N==0) RETURN
C
         ALLOCATE(BL(IPARAM(1,1)*NSCM))
         IAD1=0
         DO I=1,NSCM
            DO J=1,IPARAM(1,1)
               JJ=GRAPHE(1)%DGRAPH(1)%DDLS(J)
               BL(IAD1+J)=B(I,JJ)
            ENDDO
            IAD1=IAD1+IPARAM(1,1)
         ENDDO
         IRQTAG=MSGOFF2 + (NSPMD-1) + ISPMD
         LEN=NSCM*IPARAM(1,1)
         CALL SPMD_DS_RSEND(BL, LEN, IRQTAG, 1)
         DEALLOCATE(BL)
      ENDIF
C
      IF (IMACH/=3) THEN
         DO I=1,NSPROC
            IFILE=GRAPHE(I)%IFILE
            NDF=GRAPHE(I)%DGRAPH(1)%NDDL_F
            LEN=(NDF)*(NDF+1)/2
C Assemblage de la rigidite
            IF (IDSC==1) THEN
               ALLOCATE(KL(LEN))
               IREC=GRAPHE(I)%DGRAPH(1)%KREC
               IKC=0
               DO IR=1,LEN/10
                  READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
                  IKC=IKC+10
                  IREC=IREC+1
               ENDDO
               NR=LEN-(LEN/10)*10
               IF (NR>0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C               
               DO J=1,NDF
                  JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
                  JJ=ITAG(JJ)
                  IAD1=(J-1)*J/2
                  DO K=1,J-1
                     KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                     KK=ITAG(KK)
                     RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                     RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
                  ENDDO
                  RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
               ENDDO
               DEALLOCATE(KL)       
            ENDIF
C Assemblage des seconds membres
            DO J=1,NSCM
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  F(J,KK)=F(J,KK)+GRAPHE(I)%DGRAPH(1)%B(J,K)
               ENDDO
            ENDDO
         ENDDO
      ELSE
C SPMD - Tags :
C Requetes reelles  : matrice rigidite  : MSGOFF2 + IPROC
C                     forces            : MSGOFF2 + (NSPMD-1) + IPROC
C                     forces condensees : MSGOFF2 + 2*(NSPMD-1) + IPROC
C                     matrice masse     : MSGOFF2 + 3*(NSPMD-1) + IPROC
         IFILE=GRAPHE(1)%IFILE
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         LEN=(NDF)*(NDF+1)/2
C Assemblage de la rigidite
         IF (IDSC==1) THEN
            ALLOCATE(KL(LEN))
            IREC=GRAPHE(1)%DGRAPH(1)%KREC
            IKC=0
            DO IR=1,LEN/10
               READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
               IKC=IKC+10
               IREC=IREC+1
            ENDDO
            NR=LEN-(LEN/10)*10
            IF (NR>0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C            
            IF (ISPMD==0) THEN
C Processeur zero : recoit et assemble       
               DO J=1,NDF
                  JJ=GRAPHE(1)%DGRAPH(1)%DDLS(J)
                  JJ=GRAPHE(1)%LSDDL_GLOB(JJ)
                  JJ=ITAG(JJ)
                  IAD1=(J-1)*J/2
                  DO K=1,J-1
                     KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                     KK=GRAPHE(1)%LSDDL_GLOB(KK)
                     KK=ITAG(KK)
                     RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                     RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
                  ENDDO
                  RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
               ENDDO
               DEALLOCATE(KL)
C
               DO I=1,NSPMD-1
                  NDFP=IPARAM(1,I)
                  LENP=(NDFP)*(NDFP+1)/2
                  ALLOCATE(KL(LENP))
                  IRQTAG=MSGOFF2 + I
                  CALL SPMD_DS_RRECV(KL, LENP, IRQTAG, I+1)
                  DO J=1,NDFP
                     JJ=PDDLS(J,I)
                     JJ=ITAG(JJ)
                     IAD1=(J-1)*J/2
                     DO K=1,J-1
                        KK=PDDLS(K,I)
                        KK=ITAG(KK)
                        RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                        RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
                     ENDDO
                     RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
                  ENDDO
                  DEALLOCATE(KL)
               ENDDO
C
            ELSE
C Autres processeurs : envoient     
               IRQTAG=MSGOFF2 + ISPMD
               CALL SPMD_DS_RSEND(KL, LEN, IRQTAG, 1)
               DEALLOCATE(KL)
            ENDIF
         ENDIF
C Assemblage des seconds membres
         IF (ISPMD==0) THEN
C Processeur zero : recoit et assemble       
            DO J=1,NSCM
               DO K=1,NDF
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  F(J,KK)=F(J,KK)+GRAPHE(1)%DGRAPH(1)%B(J,K)
               ENDDO
            ENDDO
C
            ALLOCATE(BL(NSCM*NDFMAX))
            DO I=1,NSPMD-1
               IRQTAG=MSGOFF2 + 2*(NSPMD-1) + I
               LENP=IPARAM(1,I)*NSCM
               CALL SPMD_DS_RRECV(BL, LENP, IRQTAG, I+1)
               IAD1=0
               DO J=1,NSCM
                  DO K=1,IPARAM(1,I)
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     F(J,KK)=F(J,KK)+BL(IAD1+K)
                  ENDDO
                  IAD1=IAD1+IPARAM(1,I)
               ENDDO
            ENDDO
            DEALLOCATE(BL)
         ELSE
C Autres processeurs : envoient     
            ALLOCATE(BL(NSCM*IPARAM(1,1)))
            IAD1=0
            DO I=1,NSCM
               DO J=1,IPARAM(1,1)
                  BL(IAD1+J)=GRAPHE(1)%DGRAPH(1)%B(I,J)
               ENDDO
               IAD1=IAD1+IPARAM(1,1)
            ENDDO
            IRQTAG=MSGOFF2 + 2*(NSPMD-1) + ISPMD
            CALL SPMD_DS_RSEND(BL, NSCM*IPARAM(1,1), IRQTAG, 1)
            DEALLOCATE(BL)
         ENDIF
C
      ENDIF
C     
      IF (IMACH/=3.OR.ISPMD==0) THEN
C Resolution systeme lineaire pour NSCM seconds membres
         ALLOCATE(V(N,NSCM), S(N))
         IF (IDSC==1) THEN
C Block size query
            LFACW=-1
            CALL DSYTRF('U', N, RIGI, NK, IPIV, RLFACW, LFACW, INFFAC)
            LFACW=NINT(RLFACW)
            ALLOCATE(FACW(LFACW))
C Factorization
            CALL DSYTRF('U', N, RIGI, NK, IPIV, FACW, LFACW, INFFAC)
            DEALLOCATE(FACW)
         ENDIF
         DO I=1,NSCM
            DO J=1,N
               S(J)=F(I,J)
            ENDDO
            NRHS=1
            CALL DSYTRS('U', N, NRHS, RIGI, NK, IPIV, S, N, INFRES)
            IF (INFRES/=0) THEN
                CALL ANCMSG(MSGID=62,ANMODE=ANINFO)
                CALL ARRET(2)
            ENDIF
            DO J=1,N
               V(J,I)=S(J)
            ENDDO
         ENDDO
         DEALLOCATE(S)
         NDEPL=NSCM
C               
      ENDIF
C
C Redescente des deplacements
C
      IF (IMACH/=3) THEN
         DO I=1,NSPROC
            NDF=GRAPHE(I)%DGRAPH(1)%NDDL_F
            DO J=1,NDEPL
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  GRAPHE(I)%DGRAPH(1)%D(J,K)=V(KK,J)
               ENDDO
            ENDDO
         ENDDO
      ELSEIF (ISPMD==0) THEN
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         DO J=1,NDEPL
            DO K=1,NDF
               KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
               KK=GRAPHE(1)%LSDDL_GLOB(KK)
               KK=ITAG(KK)
               GRAPHE(1)%DGRAPH(1)%D(J,K)=V(KK,J)
            ENDDO
         ENDDO
C Envoie les deplacements aux autres processeurs
C Tags : 
C Requetes entieres : parametres   : MSGOFF3 + IPROC
C Requetes reelles  : deplacements : MSGOFF4 + IPROC
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF3 + I
            CALL SPMD_DS_ISEND(NDEPL, 1, IRQTAG, I+1)
C
            NDFP=IPARAM(1,I)
            ALLOCATE(VL(NDFP,NDEPL))
            DO J=1,NDEPL
               DO K=1,NDFP
                  KK=PDDLS(K,I)
                  KK=ITAG(KK)
                  VL(K,J)=V(KK,J)
               ENDDO
            ENDDO
C
            LEN=NDEPL*NDFP
            IRQTAG=MSGOFF4 + I
            CALL SPMD_DS_RSEND(VL, LEN, IRQTAG, I+1)
            DEALLOCATE(VL)
         ENDDO
         DEALLOCATE(PDDLS)
      ELSE
C Recoit les deplacements du processeur zero
         IRQTAG=MSGOFF3 + ISPMD
         CALL SPMD_DS_IRECV(NDEPL, 1, IRQTAG, 1)
C
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         ALLOCATE(VL(NDF,NDEPL))
         IRQTAG=MSGOFF4 + ISPMD
         LEN=NDEPL*NDF
         CALL SPMD_DS_RRECV(VL, LEN, IRQTAG, 1)
C
         DO J=1,NDEPL
            DO K=1,NDF
               GRAPHE(1)%DGRAPH(1)%D(J,K)=VL(K,J)
            ENDDO
         ENDDO
         DEALLOCATE(VL, PDDLS)
      ENDIF
C
      IF (IMACH/=3.OR.ISPMD==0) DEALLOCATE(V, F)
C
      RETURN
      END
Chd|====================================================================
Chd|  DSRESOL_BUCK                  source/implicit/dsolve/dsresol.F
Chd|-- called by -----------
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        DSBUCK                        source/implicit/dsolve/dsbuck.F
Chd|        SPMD_DS_IRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_ISEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_RRECV                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_DS_RSEND                 source/mpi/implicit/spmd_dsreso.F
Chd|        SPMD_IBCAST                   source/mpi/generic/spmd_ibcast.F
Chd|        DSGRAPH_MOD                   share/modules/dsgraph_mod.F   
Chd|====================================================================
      SUBROUTINE DSRESOL_BUCK(GRAPHE, NDDL, EIG, TITRE, LTITR1,
     .                        IPMESS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE DSGRAPH_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com06_c.inc"
#include      "buckcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NDDL, LTITR1, IPMESS
      my_real
     .        EIG(NBUCK,*)
      TYPE(PRGRAPH) :: GRAPHE(*)
      CHARACTER TITRE*(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, ITAG(NDDL), ITAGI(NDDL), NMOD, J, JJ, N, IADM, NN,
     .        NDFMAX, IRQTAG, MSGOFF, IPARAM(2,NSPMD-1), II,
     .        IM, IFILE, NDF, NDM, LEN, IREC, IKC, IR, NR, IAD1, K,
     .        KK, IM0, IMC, NDFP, NDMP, LENP, NM, NMMAX, MAXITR, 
     .        MAXNCV, NEV, NCV, MAXN, MAXNEV, LDV, ISHFTS, MODE, INFO,
     .        IPRI, NDEPL, NMDYN, MSGOFF2, MSGOFF3, MSGOFF4 
      my_real
     .        SHIFT, TOL
      CHARACTER WHICH*2
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: PDDLS
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: KL, KGL
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: RIGI, RIGIG, OP, V, VAL,
     .                                        VL
      DATA MSGOFF  /200000/ 
      DATA MSGOFF2 /300000/
      DATA MSGOFF3 /400000/
      DATA MSGOFF4 /500000/
C
      DO I=1,NDDL
         ITAG(I)=0
         ITAGI(I)=0
      ENDDO
C
      IF (IMACH/=3) THEN
         NMOD=0
         DO I=1,NSPROC
            DO J=1,GRAPHE(I)%DGRAPH(1)%NDDL_F
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               ITAG(JJ)=1
            ENDDO
            NMOD=NMOD+GRAPHE(I)%DGRAPH(1)%NMDYN
         ENDDO
         N=0
         DO I=1,NDDL
            N=N+ITAG(I)
            IF (N>0.AND.ITAG(I)/=0) ITAGI(N)=I
            ITAG(I)=N
         ENDDO
         IADM=N
         NN=N
         N=N+NMOD
         IF (N==0) RETURN
C
         IF (IPMESS/=0) THEN
            WRITE(IOUT,'(A)') TITRE(1:LTITR1)
            WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         ENDIF
C
         ALLOCATE(RIGI(N,N), RIGIG(N,N))
         DO I=1,N
            DO J=1,N
               RIGI(I,J)=ZERO
               RIGIG(I,J)=ZERO
            ENDDO
         ENDDO
      ELSEIF (ISPMD==0) THEN
C SPMD - Tags :
C Requetes entieres : parametres : MSGOFF + IPROC
C                     liste ddls : MSGOFF + (NSPMD-1) + IPROC
         NDFMAX=0
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + I
            CALL SPMD_DS_IRECV(IPARAM(1,I), 2, IRQTAG, I+1)
            NDFMAX=MAX(NDFMAX,IPARAM(1,I))
         ENDDO
C Tous les pros ont finis leur condensation : on affiche le titre
C de la resolution
         IF (IPMESS/=0) THEN
            WRITE(IOUT,'(A)') TITRE(1:LTITR1)
            WRITE(ISTDO,*)
            WRITE(ISTDO,'(A)') TITRE(1:LTITR1)
         ENDIF
C
         ALLOCATE(PDDLS(NDFMAX,NSPMD-1))
         DO I=1,GRAPHE(1)%DGRAPH(1)%NDDL_F
            II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
            II=GRAPHE(1)%LSDDL_GLOB(II)
            ITAG(II)=1
         ENDDO
         NMOD=GRAPHE(1)%DGRAPH(1)%NMDYN
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF + (NSPMD-1) + I
            CALL SPMD_DS_IRECV(PDDLS(1,I), IPARAM(1,I), IRQTAG, I+1)
            NDF=IPARAM(1,I)
            DO J=1,NDF
               JJ=PDDLS(J,I)
               ITAG(JJ)=1
            ENDDO
            NMOD=NMOD+IPARAM(2,I)
         ENDDO
         N=0
         DO I=1,NDDL
            N=N+ITAG(I)
            IF (N>0.AND.ITAG(I)/=0) ITAGI(N)=I
            ITAG(I)=N
         ENDDO
         IADM=N
         NN=N
         N=N+NMOD
         CALL SPMD_IBCAST(N, N, 1, 1, 0, 2)
         IF (N==0) RETURN
C
         ALLOCATE(RIGI(N,N), RIGIG(N,N))
         DO I=1,N
            DO J=1,N
               RIGI(I,J)=ZERO
               RIGIG(I,J)=ZERO
            ENDDO
         ENDDO
      ELSE
         IPARAM(1,1)=GRAPHE(1)%DGRAPH(1)%NDDL_F
         IPARAM(2,1)=GRAPHE(1)%DGRAPH(1)%NMDYN
         IRQTAG=MSGOFF + ISPMD
         CALL SPMD_DS_ISEND(IPARAM(1,1), 2, IRQTAG, 1)
         ALLOCATE(PDDLS(IPARAM(1,1),1))
         DO I=1,IPARAM(1,1)
            II=GRAPHE(1)%DGRAPH(1)%DDLS(I)
            II=GRAPHE(1)%LSDDL_GLOB(II)
            PDDLS(I,1)=II
         ENDDO
         IRQTAG=MSGOFF + (NSPMD-1) + ISPMD
         CALL SPMD_DS_ISEND(PDDLS, IPARAM(1,1), IRQTAG, 1)
C
         CALL SPMD_IBCAST(N, N, 1, 1, 0, 2)
         IF (N==0) RETURN
C
      ENDIF
C
      IF (IMACH/=3) THEN
         IM=0
         DO I=1,NSPROC
            IFILE=GRAPHE(I)%IFILE
            NDF=GRAPHE(I)%DGRAPH(1)%NDDL_F
            NDM=GRAPHE(I)%DGRAPH(1)%NMDYN
            LEN=(NDF+NDM)*(NDF+NDM+1)/2
C Assemblage de la rigidite
            ALLOCATE(KL(LEN))
            IREC=GRAPHE(I)%DGRAPH(1)%KREC
            IKC=0
            DO IR=1,LEN/10
               READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
               IKC=IKC+10
               IREC=IREC+1
            ENDDO
            NR=LEN-(LEN/10)*10
            IF (NR>0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C            
            DO J=1,NDF
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                  RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
               ENDDO
               RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
            ENDDO
            IM0=IM
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  RIGI(NN+IM,KK)=RIGI(NN+IM,KK)+KL(IAD1+K)
                  RIGI(KK,NN+IM)=RIGI(KK,NN+IM)+KL(IAD1+K)
               ENDDO
               DO K=1,IM-IM0-1
                  RIGI(NN+IM,NN+IM0+K)=
     .                       RIGI(NN+IM,NN+IM0+K)+KL(IAD1+NDF+K)
                  RIGI(NN+IM0+K,NN+IM)=
     .                       RIGI(NN+IM0+K,NN+IM)+KL(IAD1+NDF+K)
               ENDDO
               RIGI(NN+IM,NN+IM)=RIGI(NN+IM,NN+IM)+KL(IAD1+NDF+IM-IM0)
            ENDDO
            DEALLOCATE(KL)       
C Assemblage de la rigidite geometrique
            ALLOCATE(KGL(LEN))
            IREC=GRAPHE(I)%DGRAPH(1)%MREC
            IMC=0
            DO IR=1,LEN/10
               READ(IFILE,REC=IREC) (KGL(IMC+J),J=1,10)
               IMC=IMC+10
               IREC=IREC+1
            ENDDO
            NR=LEN-(LEN/10)*10
            IF (NR>0) READ(IFILE,REC=IREC) (KGL(IMC+J),J=1,NR)
C            
            DO J=1,NDF
               JJ=GRAPHE(I)%DGRAPH(1)%DDLS(J)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  RIGIG(JJ,KK)=RIGIG(JJ,KK)+KGL(IAD1+K)
                  RIGIG(KK,JJ)=RIGIG(KK,JJ)+KGL(IAD1+K)
               ENDDO
               RIGIG(JJ,JJ)=RIGIG(JJ,JJ)+KGL(IAD1+J)
            ENDDO
            IM=IM0
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  RIGIG(NN+IM,KK)=RIGIG(NN+IM,KK)+KGL(IAD1+K)
                  RIGIG(KK,NN+IM)=RIGIG(KK,NN+IM)+KGL(IAD1+K)
               ENDDO
               DO K=1,IM-IM0-1
                  RIGIG(NN+IM,NN+IM0+K)=
     .                         RIGIG(NN+IM,NN+IM0+K)+KGL(IAD1+NDF+K)
                  RIGIG(NN+IM0+K,NN+IM)=
     .                         RIGIG(NN+IM0+K,NN+IM)+KGL(IAD1+NDF+K)
               ENDDO
               RIGIG(NN+IM,NN+IM)=
     .                         RIGIG(NN+IM,NN+IM)+KGL(IAD1+NDF+IM-IM0)
            ENDDO
            DEALLOCATE(KGL)       
         ENDDO
      ELSE
C SPMD - Tags :
C Requetes reelles  : matrice rigidite  : MSGOFF2 + IPROC
C                     forces            : MSGOFF2 + (NSPMD-1) + IPROC
C                     forces condensees : MSGOFF2 + 2*(NSPMD-1) + IPROC
C                     matrice masse     : MSGOFF2 + 3*(NSPMD-1) + IPROC
         IFILE=GRAPHE(1)%IFILE
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         NDM=GRAPHE(1)%DGRAPH(1)%NMDYN
         LEN=(NDF+NDM)*(NDF+NDM+1)/2
C Assemblage de la rigidite
         ALLOCATE(KL(LEN))
         IREC=GRAPHE(1)%DGRAPH(1)%KREC
         IKC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (KL(IKC+J),J=1,10)
            IKC=IKC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR>0) READ(IFILE,REC=IREC) (KL(IKC+J),J=1,NR)
C            
         IF (ISPMD==0) THEN
C Processeur zero : recoit et assemble       
            IM=0
            DO J=1,NDF
               JJ=GRAPHE(1)%DGRAPH(1)%DDLS(J)
               JJ=GRAPHE(1)%LSDDL_GLOB(JJ)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                  RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
               ENDDO
               RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
            ENDDO
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  RIGI(NN+IM,KK)=RIGI(NN+IM,KK)+KL(IAD1+K)
                  RIGI(KK,NN+IM)=RIGI(KK,NN+IM)+KL(IAD1+K)
               ENDDO
               DO K=1,IM-1
                  RIGI(NN+IM,NN+K)=
     .                         RIGI(NN+IM,NN+K)+KL(IAD1+NDF+K)
                  RIGI(NN+K,NN+IM)=
     .                         RIGI(NN+K,NN+IM)+KL(IAD1+NDF+K)
               ENDDO
               RIGI(NN+IM,NN+IM)=RIGI(NN+IM,NN+IM)+KL(IAD1+NDF+IM)
            ENDDO
            DEALLOCATE(KL)
C
            DO I=1,NSPMD-1
               NDFP=IPARAM(1,I)
               NDMP=IPARAM(2,I)
               LENP=(NDFP+NDMP)*(NDFP+NDMP+1)/2
               ALLOCATE(KL(LENP))
               IRQTAG=MSGOFF2 + I
               CALL SPMD_DS_RRECV(KL, LENP, IRQTAG, I+1)
               DO J=1,NDFP
                  JJ=PDDLS(J,I)
                  JJ=ITAG(JJ)
                  IAD1=(J-1)*J/2
                  DO K=1,J-1
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     RIGI(JJ,KK)=RIGI(JJ,KK)+KL(IAD1+K)
                     RIGI(KK,JJ)=RIGI(KK,JJ)+KL(IAD1+K)
                  ENDDO
                  RIGI(JJ,JJ)=RIGI(JJ,JJ)+KL(IAD1+J)
               ENDDO
               IM0=IM
               DO J=1,NDMP
                  IM=IM+1
                  IAD1=(NDFP+J-1)*(NDFP+J)/2
                  DO K=1,NDFP
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     RIGI(NN+IM,KK)=RIGI(NN+IM,KK)+KL(IAD1+K)
                     RIGI(KK,NN+IM)=RIGI(KK,NN+IM)+KL(IAD1+K)
                  ENDDO
                  DO K=1,IM-IM0-1
                     RIGI(NN+IM,NN+IM0+K)=
     .                        RIGI(NN+IM,NN+IM0+K)+KL(IAD1+NDFP+K)
                     RIGI(NN+IM0+K,NN+IM)=
     .                        RIGI(NN+IM0+K,NN+IM)+KL(IAD1+NDFP+K)
                  ENDDO
                  RIGI(NN+IM,NN+IM)=
     .               RIGI(NN+IM,NN+IM)+KL(IAD1+NDFP+IM-IM0)
               ENDDO
               DEALLOCATE(KL)
            ENDDO
C
         ELSE
C Autres processeurs : envoient     
            IRQTAG=MSGOFF2 + ISPMD
            CALL SPMD_DS_RSEND(KL, LEN, IRQTAG, 1)
            DEALLOCATE(KL)
         ENDIF
C Assemblage de la rigidite geometrique
         ALLOCATE(KGL(LEN))
         IREC=GRAPHE(1)%DGRAPH(1)%MREC
         IMC=0
         DO IR=1,LEN/10
            READ(IFILE,REC=IREC) (KGL(IMC+J),J=1,10)
            IMC=IMC+10
            IREC=IREC+1
         ENDDO
         NR=LEN-(LEN/10)*10
         IF (NR>0) READ(IFILE,REC=IREC) (KGL(IMC+J),J=1,NR)
C            
         IF (ISPMD==0) THEN
C Processeur zero : recoit et assemble       
            IM=0
            DO J=1,NDF
               JJ=GRAPHE(1)%DGRAPH(1)%DDLS(J)
               JJ=GRAPHE(1)%LSDDL_GLOB(JJ)
               JJ=ITAG(JJ)
               IAD1=(J-1)*J/2
               DO K=1,J-1
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  RIGIG(JJ,KK)=RIGIG(JJ,KK)+KGL(IAD1+K)
                  RIGIG(KK,JJ)=RIGIG(KK,JJ)+KGL(IAD1+K)
               ENDDO
               RIGIG(JJ,JJ)=RIGIG(JJ,JJ)+KGL(IAD1+J)
            ENDDO
            DO J=1,NDM
               IM=IM+1
               IAD1=(NDF+J-1)*(NDF+J)/2
               DO K=1,NDF
                  KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
                  KK=GRAPHE(1)%LSDDL_GLOB(KK)
                  KK=ITAG(KK)
                  RIGIG(NN+IM,KK)=RIGIG(NN+IM,KK)+KGL(IAD1+K)
                  RIGIG(KK,NN+IM)=RIGIG(KK,NN+IM)+KGL(IAD1+K)
               ENDDO
               DO K=1,IM-1
                  RIGIG(NN+IM,NN+K)=
     .                          RIGIG(NN+IM,NN+K)+KGL(IAD1+NDF+K)
                  RIGIG(NN+K,NN+IM)=
     .                          RIGIG(NN+K,NN+IM)+KGL(IAD1+NDF+K)
               ENDDO
               RIGIG(NN+IM,NN+IM)=RIGIG(NN+IM,NN+IM)+KGL(IAD1+NDF+IM)
            ENDDO
            DEALLOCATE(KGL)       
C
            DO I=1,NSPMD-1
               NDFP=IPARAM(1,I)
               NDMP=IPARAM(2,I)
               LENP=(NDFP+NDMP)*(NDFP+NDMP+1)/2
               ALLOCATE(KGL(LENP))
               IRQTAG=MSGOFF2 + 3*(NSPMD-1) + I
               CALL SPMD_DS_RRECV(KGL, LENP, IRQTAG, I+1)
               DO J=1,NDFP
                  JJ=PDDLS(J,I)
                  JJ=ITAG(JJ)
                  IAD1=(J-1)*J/2
                  DO K=1,J-1
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     RIGIG(JJ,KK)=RIGIG(JJ,KK)+KGL(IAD1+K)
                     RIGIG(KK,JJ)=RIGIG(KK,JJ)+KGL(IAD1+K)
                  ENDDO
                  RIGIG(JJ,JJ)=RIGIG(JJ,JJ)+KGL(IAD1+J)
               ENDDO
               IM0=IM
               DO J=1,NDMP
                  IM=IM+1
                  IAD1=(NDFP+J-1)*(NDFP+J)/2
                  DO K=1,NDFP
                     KK=PDDLS(K,I)
                     KK=ITAG(KK)
                     RIGIG(NN+IM,KK)=RIGIG(NN+IM,KK)+KGL(IAD1+K)
                     RIGIG(KK,NN+IM)=RIGIG(KK,NN+IM)+KGL(IAD1+K)
                  ENDDO
                  DO K=1,IM-IM0-1
                     RIGIG(NN+IM,NN+IM0+K)=
     .                        RIGIG(NN+IM,NN+IM0+K)+KGL(IAD1+NDFP+K)
                     RIGIG(NN+IM0+K,NN+IM)=
     .                        RIGIG(NN+IM0+K,NN+IM)+KGL(IAD1+NDFP+K)
                  ENDDO
                  RIGIG(NN+IM,NN+IM)=
     .               RIGIG(NN+IM,NN+IM)+KGL(IAD1+NDFP+IM-IM0)
               ENDDO
               DEALLOCATE(KGL)
            ENDDO
         ELSE
C Autres processeurs : envoient     
            IRQTAG=MSGOFF2 + 3*(NSPMD-1) + ISPMD
            CALL SPMD_DS_RSEND(KGL, LEN, IRQTAG, 1)
            DEALLOCATE(KGL)
         ENDIF
C
      ENDIF
C     
      IF (IMACH/=3.OR.ISPMD==0) THEN
         NM=BINCV
         NMMAX=BMAXNCV
         MAXITR=BNITER
         MAXNCV=NMMAX*NBUCK
         SHIFT=SHFTBUCK
         NEV=NBUCK
         NCV=MIN(N,NM*NEV)
         MAXN=N
         MAXNEV=NEV
         LDV=MAXN
         WHICH='LM'
         ISHFTS=1
         MODE=4
         INFO=0
         TOL=ZERO
         IPRI=BIPRI
         ALLOCATE(OP(N,N), V(N,MAXNCV), VAL(MAXNCV,2))
         VAL = 0
         DO I=1,N
            DO J=1,N
               OP(I,J)=RIGI(I,J)-SHIFT*RIGIG(I,J)
            ENDDO
         ENDDO
#ifdef WITHOUT_LINALG
      WRITE(6,*) "Error: compiled with WITHOUT_LINALG"
      CALL FLUSH(6)
      CALL ARRET(5)
#else

         CALL DSBUCK(N,      NEV,    NCV,    WHICH, INFO,
     .               MAXN,   MAXNEV, MAXNCV, LDV,   ISHFTS,
     .               MAXITR, MODE,   TOL,    OP,    RIGI,
     .               RIGIG,  VAL,    V,      IPRI,  SHIFT )
#endif
         DO I=1,NBUCK
            EIG(I,1)=VAL(I,1)
            EIG(I,2)=VAL(I,2)
         ENDDO   
         WRITE(IOUT,*)
         WRITE(IOUT,'(A6,1PG11.4,A35,I10)') 
     .    'SHIFT ',SHIFT,' NUMBER OF BUCKLING CRITICAL LOADS ',NBUCK
         WRITE(IOUT,'(A)') ' CRITICAL LOADS:'
         WRITE(IOUT,'(A)') '         NUMBER  CRITICAL LOAD'
         DO I=1,NBUCK
            WRITE(IOUT,'(5X,I10,4X,1PG11.4)') I,EIG(I,1)
         ENDDO
         WRITE(IOUT,*)
         NDEPL=NBUCK
      ENDIF
C
C Redescente des deplacements
C
      IF (IMACH/=3) THEN
         IM0=0
         DO I=1,NSPROC
            NDF=GRAPHE(I)%DGRAPH(1)%NDDL_F
            NMDYN=GRAPHE(I)%DGRAPH(1)%NMDYN
            DO J=1,NDEPL
               DO K=1,NDF
                  KK=GRAPHE(I)%DGRAPH(1)%DDLS(K)
                  KK=ITAG(KK)
                  GRAPHE(I)%DGRAPH(1)%D(J,K)=V(KK,J)
               ENDDO
               IM=IM0
               DO K=1,NMDYN
                  IM=IM+1
                  GRAPHE(I)%DGRAPH(1)%DM(J,K)=V(IADM+IM,J)
               ENDDO
            ENDDO
            IM0=IM0+NMDYN
         ENDDO
      ELSEIF (ISPMD==0) THEN
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         NMDYN=GRAPHE(1)%DGRAPH(1)%NMDYN
         DO J=1,NDEPL
            DO K=1,NDF
               KK=GRAPHE(1)%DGRAPH(1)%DDLS(K)
               KK=GRAPHE(1)%LSDDL_GLOB(KK)
               KK=ITAG(KK)
               GRAPHE(1)%DGRAPH(1)%D(J,K)=V(KK,J)
            ENDDO
            IM=0
            DO K=1,NMDYN
               IM=IM+1
               GRAPHE(1)%DGRAPH(1)%DM(J,K)=V(IADM+IM,J)
            ENDDO
         ENDDO
         IADM=IADM+NMDYN
C Envoie les deplacements aux autres processeurs
C Tags : 
C Requetes entieres : parametres   : MSGOFF3 + IPROC
C Requetes reelles  : deplacements : MSGOFF4 + IPROC
         DO I=1,NSPMD-1
            IRQTAG=MSGOFF3 + I
            CALL SPMD_DS_ISEND(NDEPL, 1, IRQTAG, I+1)
C
            NDFP=IPARAM(1,I)
            NDMP=IPARAM(2,I)
            ALLOCATE(VL(NDFP+NDMP,NDEPL))
            DO J=1,NDEPL
               DO K=1,NDFP
                  KK=PDDLS(K,I)
                  KK=ITAG(KK)
                  VL(K,J)=V(KK,J)
               ENDDO
               DO K=1,NDMP
                  VL(NDFP+K,J)=V(IADM+K,J)
               ENDDO
            ENDDO
            IADM=IADM+NDMP
C
            LEN=NDEPL*(NDFP+NDMP)
            IRQTAG=MSGOFF4 + I
            CALL SPMD_DS_RSEND(VL, LEN, IRQTAG, I+1)
            DEALLOCATE(VL)
         ENDDO
         DEALLOCATE(PDDLS)
      ELSE
C Recoit les deplacements du processeur zero
         IRQTAG=MSGOFF3 + ISPMD
         CALL SPMD_DS_IRECV(NDEPL, 1, IRQTAG, 1)
C
         NDF=GRAPHE(1)%DGRAPH(1)%NDDL_F
         NMDYN=GRAPHE(1)%DGRAPH(1)%NMDYN
         ALLOCATE(VL(NDF+NMDYN,NDEPL))
         IRQTAG=MSGOFF4 + ISPMD
         LEN=NDEPL*(NDF+NMDYN)
         CALL SPMD_DS_RRECV(VL, LEN, IRQTAG, 1)
C
         DO J=1,NDEPL
            DO K=1,NDF
               GRAPHE(1)%DGRAPH(1)%D(J,K)=VL(K,J)
            ENDDO
            DO K=1,NMDYN
               GRAPHE(1)%DGRAPH(1)%DM(J,K)=VL(NDF+K,J)
            ENDDO
         ENDDO
         DEALLOCATE(VL, PDDLS)
      ENDIF
C
      IF (IMACH/=3.OR.ISPMD==0) THEN
         DEALLOCATE(V)
C
         DEALLOCATE(RIGI, RIGIG)
      ENDIF
C
      RETURN
      END
