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
Chd|  W_CLUSTER                     source/output/cluster/w_cluster.F
Chd|-- called by -----------
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        WRITE_DB                      source/output/tools/write_db.F
Chd|        WRITE_I_C                     source/output/tools/sortie_c.c
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|====================================================================
      SUBROUTINE W_CLUSTER(CLUSTER)
C-----------------------------------------------
C  Description:
C        -IN: Array of CLUSTER structures 
C        -OUT: nothing
C  Writes the structure into the restart file
C  (reading done READ_CLUSTER)
C-----------------------------------------------
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTER
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,K, IEL, NEL,NELG, NNOD
      INTEGER II,IFAIL,ITY,ID,RL,IL
      INTEGER ILCLUSTER(NCLUSTER),RLCLUSTER(NCLUSTER)
      INTEGER RLCLUSTER_MAX,ILCLUSTER_MAX
      my_real,
     .   DIMENSION(:), ALLOCATABLE :: RCLTAB  
      INTEGER, DIMENSION(:), ALLOCATABLE :: ICLTAB
C=======================================================================

      IF(NCLUSTER > 0) THEN
        ILCLUSTER(1:NCLUSTER)  = 0 
        RLCLUSTER(1:NCLUSTER) =  ZERO
      ENDIF
      RLCLUSTER_MAX = 0
      ILCLUSTER_MAX = 0
   
      DO I = 1, NCLUSTER
        ITY   = CLUSTER(I)%TYPE
        NEL   = CLUSTER(I)%NEL
        NNOD  = CLUSTER(I)%NNOD
        IFAIL = CLUSTER(I)%IFAIL
        ILCLUSTER(I) = 8 + 2 * (NEL + NNOD)
        RLCLUSTER(I) = 1 
        IF (IFAIL > 0) RLCLUSTER(I) = RLCLUSTER(I) + 4 
        IF (IFAIL == 3) RLCLUSTER(I) = RLCLUSTER(I) + 8  
        IF ( RLCLUSTER_MAX < RLCLUSTER(I) ) THEN 
          RLCLUSTER_MAX = RLCLUSTER(I)
        ENDIF 
        IF ( ILCLUSTER_MAX < ILCLUSTER(I) ) THEN 
          ILCLUSTER_MAX = ILCLUSTER(I)
        ENDIF 
      ENDDO !NCLUSTER

      IF(NCLUSTER > 0) THEN
        CALL WRITE_I_C(ILCLUSTER,NCLUSTER)
        CALL WRITE_I_C(RLCLUSTER,NCLUSTER)
        ALLOCATE (ICLTAB(ILCLUSTER_MAX)) ! Integer CLuster TABle
        ALLOCATE (RCLTAB(RLCLUSTER_MAX)) ! Real CLuster TABle
      ENDIF

      DO I = 1, NCLUSTER
        ICLTAB(1:ILCLUSTER(I)) = 0 
        RCLTAB(1:RLCLUSTER(I)) = ZERO
        IL = 0
        RL = 0
        ICLTAB(IL+1) = CLUSTER(I)%ID
        ICLTAB(IL+2) = CLUSTER(I)%TYPE
        ICLTAB(IL+3) = CLUSTER(I)%IFAIL
        ICLTAB(IL+4) = CLUSTER(I)%IGR
        ICLTAB(IL+5) = CLUSTER(I)%NEL
        ICLTAB(IL+6) = CLUSTER(I)%NNOD
        ICLTAB(IL+7) = CLUSTER(I)%SKEW 
        ICLTAB(IL+8) = CLUSTER(I)%OFF
        IL = IL + 8
        RCLTAB(RL+1) = CLUSTER(I)%FAIL
        RL = RL + 1
        NEL  = CLUSTER(I)%NEL
        NNOD = CLUSTER(I)%NNOD
c
        DO J = 1,NEL
          ID = CLUSTER(I)%ELEM(J)
          ICLTAB(IL + J)     = CLUSTER(I)%NG(J) !element local group number
          ICLTAB(IL + J+NEL) = ID ! element index in the group
        ENDDO  ! J = 1,NEL
c
        IL = IL + NEL*2      
        DO J = 1,NNOD
          ICLTAB(IL + J)      = CLUSTER(I)%NOD1(J)
          ICLTAB(IL + J+NNOD) = CLUSTER(I)%NOD2(J)
        ENDDO

        IL = IL + NNOD*2      
        IF (CLUSTER(I)%IFAIL > 0) THEN
          RCLTAB(RL + 1) = CLUSTER(I)%FMAX(1)
          RCLTAB(RL + 2) = CLUSTER(I)%FMAX(2)
          RCLTAB(RL + 3) = CLUSTER(I)%MMAX(1)
          RCLTAB(RL + 4) = CLUSTER(I)%MMAX(2)
          RL = RL + 4     
        ENDIF

        IF (CLUSTER(I)%IFAIL == 3) THEN
          RCLTAB(RL + 1) = CLUSTER(I)%AX(1)
          RCLTAB(RL + 2) = CLUSTER(I)%AX(2)
          RCLTAB(RL + 3) = CLUSTER(I)%AX(3)
          RCLTAB(RL + 4) = CLUSTER(I)%AX(4)
          RCLTAB(RL + 5) = CLUSTER(I)%NX(1)
          RCLTAB(RL + 6) = CLUSTER(I)%NX(2)
          RCLTAB(RL + 7) = CLUSTER(I)%NX(3)
          RCLTAB(RL + 8) = CLUSTER(I)%NX(4)
        ENDIF

        CALL WRITE_DB(RCLTAB,RLCLUSTER(I))
        CALL WRITE_I_C(ICLTAB,ILCLUSTER(I))

      ENDDO  !  I = 1, NCLUSTER

      IF(NCLUSTER > 0) THEN
        DEALLOCATE (RCLTAB)
        DEALLOCATE (ICLTAB)
      ENDIF


C-----------
      RETURN
      END
