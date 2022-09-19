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
C
Chd|====================================================================
Chd|  C_VOIS                        source/restart/ddsplit/c_vois.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        NLOCAL                        source/spmd/node/ddtools.F    
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        SPLIT_CFD_MOD                 share/modules1/split_cfd_mod.F
Chd|====================================================================
      SUBROUTINE C_VOIS(NSVOIS  ,NQVOIS ,NTGVOIS, NRCVVOIS,NSNDVOIS,ILAW11 ,
     2                  NSEGFL_L,IPARG  ,CEP     ,ALE_CONNECTIVITY  ,NUMEL  ,
     3                  PROC    ,IXS    ,IXQ     ,IXTG,
     4                  NERVOIS ,NESVOIS,NODLOCAL,NUMNOD_L,NUMEL_L,
     5                  CEL, MULTI_FVM,ALE_ELM,SIZE_ALE_ELM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE FRONT_MOD
      USE MULTI_FVM_MOD
      USE SPLIT_CFD_MOD
      USE ALE_CONNECTIVITY_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSVOIS,NQVOIS,NTGVOIS,NRCVVOIS,NSNDVOIS,ILAW11,PROC,NUMEL,
     .        NERVOIS,NESVOIS,NUMNOD_L,NUMEL_L,
     .        NSEGFL_L,IPARG(NPARG,*),CEP(*),NODLOCAL(*),
     .        IXS(NIXS,*), IXQ(NIXQ,*), IXTG(NIXTG, *),
     .        CEL(*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      INTEGER, INTENT(IN) :: SIZE_ALE_ELM
      TYPE(split_cfd_type), INTENT(IN) :: ALE_ELM
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, K, N, NG, IE_LOC, IV_LOC,
     .        ISOLNOD,ILAW,JTUR,JTHE,ITY,IE,NFT,NEL,IV,PROC2, P, PP,
     .        L,NSOL,NS,ISILENT,IJK

      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGE,TAG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAGE_L,TAG_L,TAGS
      LOGICAL PSEARCH
      INTEGER IAD, IAD1, LGTH
C-----------------------------------------------
!     allocate 1d array
      ALLOCATE( TAGE(NUMEL),TAG(NUMNOD) )
!     allocate 2d array
      ALLOCATE( TAGE_L(NSPMD,NUMEL_L),TAG_L(NSPMD,NUMNOD_L) )
      ALLOCATE( TAGS(NSPMD,SEGINDX) )
! ------------------------------------
C
      DO I = 1, NUMNOD
        TAG(I) = 0
      ENDDO
      DO I = 1, NUMEL
        TAGE(I) = 0
      ENDDO
C
      TAGE_L(1:NSPMD,1:NUMEL_L) = 0
      TAG_L(1:NSPMD,1:NUMNOD_L) = 0

      DO P = 1, NSPMD
        IF(P/=PROC+1)THEN
          DO I = 1, NUMEL_L
            TAGE_L(P,I) = 0
          END DO
          DO I = 1, NUMNOD_L
            TAG_L(P,I) = 0
          END DO
        END IF
        DO I = 1, SEGINDX
          TAGS(P,I) = 0
        END DO
      END DO
C
      DO NG=1,NGROUP
       ILAW    = IPARG(1,NG)
       ISILENT = IPARG(64,NG)
       IF(ILAW==11 .OR. ISILENT==1) ILAW11 = 1
       JTUR=IPARG(12,NG)
       JTHE=IPARG(13,NG)
       IF(IPARG(32,NG)==PROC) THEN
         NEL = IPARG(2,NG)
         NFT = IPARG(3,NG)
         ITY = IPARG(5,NG)
Cel voir autres types solides
         ISOLNOD = IPARG(28,NG)
         IF(ITY==1) THEN
           DO I = 1, NEL
             IE = I+NFT
             IE_LOC = CEL(IE)
             IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
Cel IVOIS
             DO J = 1, LGTH
               IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
               IF (IV>0) THEN
                 PROC2 = CEP(IV)
                 IF(PROC2/=PROC) THEN
                   IF(TAGE(IV)==0) THEN
                     TAGE(IV) = 1
                     NSVOIS = NSVOIS + 1
                     NERVOIS=NERVOIS+1
                   ENDIF
                   IF(TAGE_L(PROC2+1,IE_LOC)==0)THEN
                     NESVOIS = NESVOIS+1
                     TAGE_L(PROC2+1,IE_LOC)=1
                   END IF
                 ENDIF
               ELSEIF(PROC/=0.AND.IV<0)THEN
C IV < 0 : SEGMENT INT12
                 IF(TAGS(1,-IV)==0)THEN
                   NSEGFL_L = NSEGFL_L + 1
                   TAGS(1,-IV) = 1
                 ENDIF
               ENDIF
             ENDDO
           ENDDO
         ELSEIF(ITY==2) THEN
Cel 2D
            DO I = 1, NEL
               IE = I+NFT
               IE_LOC = CEL(IE)
               IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
               LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
               DO J = 1, LGTH
                  IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
                  IF (IV>0) THEN
                     PROC2 = CEP(IV)
                     IF(PROC2/=PROC) THEN
                        IF(TAGE(IV)==0) THEN
                           TAGE(IV) = 1
                           NQVOIS = NQVOIS + 1
                           NERVOIS=NERVOIS+1
                        END IF
                        IF(TAGE_L(PROC2+1,IE_LOC)==0)THEN
                           NESVOIS = NESVOIS+1
                           TAGE_L(PROC2+1,IE_LOC)=1
                        END IF
                     ENDIF
                  ENDIF
               ENDDO
            ENDDO
        ELSEIF (ITY == 7 .AND. (N2D /= 0 .AND. MULTI_FVM%IS_USED)) THEN
C     2D Triangulaire
           DO I = 1, NEL
              IE = I + NFT
              IE_LOC = CEL(IE)
              IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
              LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
              DO J = 1, LGTH
                 IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
                 IF (IV > 0) THEN
                    PROC2 = CEP(IV)
                    IF (PROC2 /= PROC) THEN
                       IF(TAGE(IV) == 0) THEN
                           TAGE(IV) = 1
                           NTGVOIS = NTGVOIS + 1
                           NERVOIS = NERVOIS + 1
                        ENDIF
                        IF(TAGE_L(PROC2 + 1, IE_LOC) == 0)THEN
                           NESVOIS = NESVOIS + 1
                           TAGE_L(PROC2 + 1, IE_LOC) = 1
                        ENDIF
                    ENDIF
                 ENDIF
              ENDDO
           ENDDO
         ENDIF
C
       ELSEIF(SEGINDX>0.AND.PROC==0)THEN      ! IPARG(32,NG)+1/=PROC
         NEL = IPARG(2,NG)
         NFT = IPARG(3,NG)
         ITY = IPARG(5,NG)
         P = IPARG(32,NG)+1
Cel voir autres types solides
         ISOLNOD = IPARG(28,NG)
         IF(ITY==1) THEN
           DO I = 1, NEL
             IE = I+NFT
Cel IVOIS
             IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(IE+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
             DO J = 1, LGTH
               IV = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
               IF(IV<0)THEN
C IV < 0 : SEGMENT INT12
                 IF(TAGS(P,-IV)==0)THEN
                   NSEGFL_L = NSEGFL_L + 1
                   TAGS(P,-IV) = 1
                 END IF
               END IF
             END DO
           END DO
         END IF
       ENDIF
      ENDDO
C
C
C Traitement supplementaire Couplage Fluide/Structure
!     because it increases cpu time
      DO I = 1, NUMELS
        IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
        LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-IAD1
        DO L=1,8
         NS = IXS(L+1,I)
         IF(NLOCAL(NS,PROC+1)==1) THEN
          DO J = 1, LGTH
             IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
             IF (IE>0) then
              DO K=1,8
               N = IXS(K+1,IE)
               IF(TAG(N)==0) THEN
                 IF (NODLOCAL(N)==0)THEN
                     NRCVVOIS = NRCVVOIS + 1
                     TAG(N) = 1
                     NODLOCAL(N)=NUMNOD_L+NRCVVOIS
                  END IF   ! nlocal(n) = 0
               ENDIF       ! tag(n) = 0
              ENDDO
             END IF
          END DO
         END IF
        ENDDO
      ENDDO
C
      DO IJK=1,SIZE_ALE_ELM
       I = ALE_ELM%SOL_ID(IJK)
       IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
       LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-ALE_CONNECTIVITY%ee_connect%iad_connect(I)
       DO L = 1,8
         NSOL = IXS(L+1,I)
         DO J= 1,LGTH
           IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
           IF (IE>0) THEN
             DO K=1,8
               N = IXS(K+1,IE)
               IF(NODLOCAL(N)>0 .AND. NODLOCAL(N) <= NUMNOD_L)THEN
                 IF( IFRONT%IENTRY(N) /=0) THEN
                   IF( IFRONT%P(1,IFRONT%IENTRY(N)) < PROC+1) GOTO 1111
                 ENDIF
                 PSEARCH = .TRUE.
                 IAD = IFRONT%IENTRY(NSOL)
                 IF(IAD <= 0) PSEARCH = .FALSE.
                 DO WHILE(PSEARCH)
                   P = IFRONT%P(1,IAD)
                   IF(TAG_L(P,NODLOCAL(N))==0.AND.P/=PROC+1) THEN
                     IF(NLOCAL(N,P)==0) THEN
                         NSNDVOIS = NSNDVOIS + 1
                         TAG_L(P,NODLOCAL(N)) = 1
                     END IF  ! nlocal(n) = 0
                   ENDIF ! tag_l(p) == 0 .and. ...
                   IF(IFRONT%P(2,IAD)==0) PSEARCH = .FALSE.
                   IAD = IFRONT%P(2,IAD)
                 END DO
 1111          CONTINUE
               ENDIF
             ENDDO
           ENDIF
         ENDDO
       ENDDO
      END DO

      DO I = 1, NUMELQ
        IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
        LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-IAD1
        DO L=1,4
         NS = IXQ(L+1,I)
         IF(NLOCAL(NS,PROC+1)==1) THEN
          DO J = 1, LGTH
             IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
             IF (IE>0) then
              DO K=1,4
               N = IXQ(K+1,IE)
               IF(TAG(N)==0) THEN
                IF (NODLOCAL(N)==0)THEN
                 NRCVVOIS = NRCVVOIS + 1
                 TAG(N) = 1
                 NODLOCAL(N)=NUMNOD_L+NRCVVOIS
                END IF   ! nlocal(n) = 0
               ENDIF     ! tag(n) = 0
              ENDDO
             END IF
          END DO
         END IF
        ENDDO
C
       DO L = 1,4
         NSOL = IXQ(L+1,I)
         DO J= 1,LGTH
           IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
           IF (IE>0) THEN
            DO K=1,4
              N = IXQ(K+1,IE)
              IF(NODLOCAL(N)>0 .AND. NODLOCAL(N) <= NUMNOD_L)THEN
                DO PROC2 = 1, PROC
                  IF(NLOCAL(N,PROC2)==1)GOTO 1112
                END DO
                DO P=1,NSPMD
                  IF(TAG_L(P,NODLOCAL(N))==0.AND.P/=PROC+1) THEN
                   IF(NLOCAL(NSOL,P)==1) THEN
                    IF(NLOCAL(N,P)==0) THEN
                     NSNDVOIS = NSNDVOIS + 1
                     TAG_L(P,NODLOCAL(N)) = 1
                    ENDIF   ! nlocal(n) = 0
                   ENDIF    ! nlocal(nsol)
                  END IF    ! tag_l(p).and. ...
                END DO
 1112         CONTINUE
              ENDIF
            ENDDO
           ENDIF
         ENDDO
       ENDDO
      END DO

C     Triangle 2D

      IF (N2D /= 0 .AND. MULTI_FVM%IS_USED) THEN
      DO I = 1, NUMELTG
        IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(I)
        LGTH = ALE_CONNECTIVITY%ee_connect%iad_connect(I+1)-IAD1
        DO L=1,3
         NS = IXTG(L+1,I)
         IF(NLOCAL(NS,PROC+1)==1) THEN
          DO J = 1, LGTH
             IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
             IF (IE>0) then
              DO K=1,3
               N = IXTG(K+1,IE)
               IF(TAG(N)==0) THEN
                IF (NLOCAL(N,PROC+1)==0)THEN
                 NRCVVOIS = NRCVVOIS + 1
                 TAG(N) = 1
                 NODLOCAL(N)=NUMNOD_L+NRCVVOIS
                END IF   ! nlocal(n) = 0
               ENDIF     ! tag(n) = 0
              ENDDO
             END IF
          END DO
         END IF
        ENDDO
C
       DO L = 1,3
         NSOL = IXTG(L+1,I)
         DO J= 1,LGTH
           IE = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + J - 1)
           IF (IE>0) THEN
            DO K=1,3
              N = IXTG(K+1,IE)
              IF(NLOCAL(N,PROC+1)==1)THEN
                DO PROC2 = 1, PROC
                  IF(NLOCAL(N,PROC2)==1)GOTO 1113
                END DO
                DO P=1,NSPMD
                  IF(TAG_L(P,NODLOCAL(N))==0.AND.P/=PROC+1) THEN
                   IF(NLOCAL(NSOL,P)==1) THEN
                    IF(NLOCAL(N,P)==0) THEN
                     NSNDVOIS = NSNDVOIS + 1
                     TAG_L(P,NODLOCAL(N)) = 1
                    ENDIF   ! nlocal(n) = 0
                   ENDIF    ! nlocal(nsol)
                  END IF    ! tag_l(p).and. ...
                END DO
 1113         CONTINUE
              ENDIF
            ENDDO
           ENDIF
         ENDDO
       ENDDO
      END DO
      ENDIF
C
! ------------------------------------
!     deallocate 1d array
      DEALLOCATE( TAGE,TAG )
!     deallocate 2d array
      DEALLOCATE( TAGE_L,TAG_L )
      DEALLOCATE( TAGS )
! ------------------------------------
      RETURN
      END
