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
Chd|  W_CLUSTER                     source/restart/ddsplit/w_cluster.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_I_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|====================================================================
      SUBROUTINE W_CLUSTER(CLUSTER,IPARG,NODLOCAL,
     .                     NCLUSTER_L,CEP,PROC,
     .                     NUMLOCGROUP, LEN_IA,LEN_AM )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CLUSTER_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER CEP(*),NODLOCAL(*),IPARG(NPARG,*)
      INTEGER NCLUSTER_L,PROC,LEN_IA,LEN_AM
      INTEGER :: NUMLOCGROUP(NGROUP)
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTER
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,ID,NEL,NG,NELG,NNOD,NFT,ITY,IFAIL
      INTEGER IL, RL
      INTEGER ESHIFT(3), ILCLUSTER_MAX, RLCLUSTER_MAX
      my_real, DIMENSION(:), ALLOCATABLE :: RCLTAB
      INTEGER, DIMENSION(:), ALLOCATABLE :: ICLTAB,INDX,ILCLUSTER,RLCLUSTER

C=======================================================================
      ESHIFT(1) = 0                                   ! brick  cluster                                  
      ESHIFT(2) = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP  ! spring cluster
      ESHIFT(3) = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP  ! spring cluster special 
      ALLOCATE(INDX(NCLUSTER_L)) ! INDeXes of local clusters 
      ALLOCATE(ILCLUSTER(NCLUSTER_L)) ! Integer Lengths of local CLUSTERs
      ALLOCATE(RLCLUSTER(NCLUSTER_L)) ! Real Lengths of local CLUSTERs
      J = 0
      ILCLUSTER(1:NCLUSTER_L)  = 0 
      RLCLUSTER(1:NCLUSTER_L) =  ZERO
      RLCLUSTER_MAX = 0
      ILCLUSTER_MAX = 0

      DO I = 1, NCLUSTER
        ITY   = CLUSTER(I)%TYPE
        IF ( CEP(CLUSTER(I)%ELEM(1) + ESHIFT(ITY)) ==  PROC ) THEN
          J = J + 1
          NEL   = CLUSTER(I)%NEL
          NNOD  = CLUSTER(I)%NNOD
          IFAIL = CLUSTER(I)%IFAIL
          ILCLUSTER(J) = 8 + 2 * (NEL + NNOD)
          RLCLUSTER(J) = 1 
          IF (IFAIL > 0) RLCLUSTER(J) = RLCLUSTER(J) + 4 
          IF (IFAIL == 3) RLCLUSTER(J) = RLCLUSTER(J) + 8  
          INDX(J) = I
          IF ( RLCLUSTER_MAX < RLCLUSTER(J) ) THEN 
            RLCLUSTER_MAX = RLCLUSTER(J)
          ENDIF 
          IF ( ILCLUSTER_MAX < ILCLUSTER(J) ) THEN 
            ILCLUSTER_MAX = ILCLUSTER(J)
          ENDIF 
        ENDIF
      ENDDO
c-----
      CALL WRITE_I_C(ILCLUSTER,NCLUSTER_L)
      LEN_IA = LEN_IA + NCLUSTER_L   
      CALL WRITE_I_C(RLCLUSTER,NCLUSTER_L)
      LEN_IA = LEN_IA + NCLUSTER_L   
c-----
      ALLOCATE (ICLTAB(ILCLUSTER_MAX)) ! Integer CLuster TABle
      ALLOCATE (RCLTAB(RLCLUSTER_MAX)) ! Real CLuster TABle
      DO I = 1, NCLUSTER_L
        II = INDX(I) ! global number of the cluster

!        ICLTAB(1:ILCLUSTER(I)) = 0 
!        RCLTAB(1:RLCLUSTER(I)) = ZERO
 
        IL = 0
        RL = 0
        ICLTAB(IL+1) = CLUSTER(II)%ID
        ICLTAB(IL+2) = CLUSTER(II)%TYPE
        ICLTAB(IL+3) = CLUSTER(II)%IFAIL
        ICLTAB(IL+4) = CLUSTER(II)%IGR
        ICLTAB(IL+5) = CLUSTER(II)%NEL
        ICLTAB(IL+6) = CLUSTER(II)%NNOD
        ICLTAB(IL+7) = CLUSTER(II)%SKEW 
        ICLTAB(IL+8) = CLUSTER(II)%OFF
        IL = IL + 8
        RCLTAB(RL+1) = CLUSTER(II)%FAIL
        RL = RL + 1
        NEL  = CLUSTER(II)%NEL
        NNOD = CLUSTER(II)%NNOD
c
        DO J = 1,NEL
          ID = CLUSTER(II)%ELEM(J)
          DO NG = 1,NGROUP
            NELG = IPARG(2,NG)
            NFT  = IPARG(3,NG)
            ITY  = IPARG(5,NG)
            IF (ITY == 1 .AND. CLUSTER(II)%TYPE == 1) THEN
              K = ID - NFT
              IF (K <= NELG) GOTO 100
            ELSEIF (ITY == 6 .AND. CLUSTER(II)%TYPE == 2) THEN
              K = ID - NFT
              IF (K <= NELG) GOTO 100
            ENDIF 
          ENDDO  ! NG = 1,NGROUP
 100      CONTINUE
          ICLTAB(IL + J)     = NUMLOCGROUP(NG) !element local group number
          ICLTAB(IL + J+NEL) = K  ! element index in the group
        ENDDO  ! J = 1,NEL
c
        IL = IL + NEL*2      
        DO J = 1,NNOD
          ICLTAB(IL + J)      = NODLOCAL(CLUSTER(II)%NOD1(J))
          ICLTAB(IL + J+NNOD) = NODLOCAL(CLUSTER(II)%NOD2(J))
        ENDDO
        IL = IL + NNOD*2     
c------------------------------    
        IF (CLUSTER(II)%IFAIL > 0) THEN
          RCLTAB(RL + 1) = CLUSTER(II)%FMAX(1)
          RCLTAB(RL + 2) = CLUSTER(II)%FMAX(2)
          RCLTAB(RL + 3) = CLUSTER(II)%MMAX(1)
          RCLTAB(RL + 4) = CLUSTER(II)%MMAX(2)
          RL = RL + 4     
        ENDIF
        IF (CLUSTER(II)%IFAIL == 3) THEN
          RCLTAB(RL + 1) = CLUSTER(II)%AX(1)
          RCLTAB(RL + 2) = CLUSTER(II)%AX(2)
          RCLTAB(RL + 3) = CLUSTER(II)%AX(3)
          RCLTAB(RL + 4) = CLUSTER(II)%AX(4)
          RCLTAB(RL + 5) = CLUSTER(II)%NX(1)
          RCLTAB(RL + 6) = CLUSTER(II)%NX(2)
          RCLTAB(RL + 7) = CLUSTER(II)%NX(3)
          RCLTAB(RL + 8) = CLUSTER(II)%NX(4)
          RL = RL + 8
        ENDIF
!       ------------------        
!       check if I/R CLTAB is fully initialized
        IF(IL< ILCLUSTER(I)) ICLTAB(IL+1:ILCLUSTER(I)) = 0 
        IF(RL<RLCLUSTER(I)) RCLTAB(RL+1:RLCLUSTER(I)) = ZERO
!       ------------------
c-----
        CALL WRITE_DB(RCLTAB,RLCLUSTER(I))
        LEN_AM = LEN_AM + RLCLUSTER(I)
        CALL WRITE_I_C(ICLTAB,ILCLUSTER(I))
        LEN_IA = LEN_IA + ILCLUSTER(I)

c-----
      ENDDO  !  I = 1, NCLUSTER_L
      DEALLOCATE (INDX)
      DEALLOCATE (ILCLUSTER)
      DEALLOCATE (RLCLUSTER)
      DEALLOCATE (RCLTAB)
      DEALLOCATE (ICLTAB)

C-----------
      RETURN
      END

Chd|====================================================================
Chd|  C_CLUSTER                     source/restart/ddsplit/w_cluster.F
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- calls ---------------
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|====================================================================
      SUBROUTINE C_CLUSTER(CLUSTERS,PROC,CEP,NCLUSTER_L,CLUSTERS_ID_L)
C Count the number of clusters per PROC
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CLUSTER_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PROC, NCLUSTER_L
      INTEGER CEP(*), CLUSTERS_ID_L(NCLUSTER)
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER  I,OFF 
C=======================================================================
      NCLUSTER_L = 0
      CLUSTERS_ID_L(1:NCLUSTER) = 0
      DO I = 1,NCLUSTER 
        OFF = 0
        IF (CLUSTERS(I)%TYPE /= 1) THEN
          OFF = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP 
        END IF
        IF (CEP(CLUSTERS(I)%ELEM(1)+OFF) == PROC) THEN
          NCLUSTER_L = NCLUSTER_L + 1
c    Local ID of the Ith global cluster
          CLUSTERS_ID_L(I) = NCLUSTER_L 
        END IF
      END DO
      RETURN
      END
Chd|====================================================================
Chd|  APPLYSORT2CLUSTER             source/restart/ddsplit/w_cluster.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|====================================================================
      SUBROUTINE APPLYSORT2CLUSTER(CLUSTERS,PERMUTATIONS)
C Apply the new numbering (given in permutations) to the elements of clusters
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE CLUSTER_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER PERMUTATIONS(*)
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER  I, J, OFFSET
C=======================================================================

      DO I = 1, NCLUSTER 
        OFFSET = 0
        IF (CLUSTERS(I)%TYPE /= 1) THEN
          OFFSET = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP 
        END IF
        DO J = 1, CLUSTERS(I)%NEL
          CLUSTERS(I)%ELEM(J) = PERMUTATIONS(CLUSTERS(I)%ELEM(J))
        END DO
      END DO
      RETURN
      END
