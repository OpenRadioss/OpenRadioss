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
Chd|  READ_CLUSTER                  source/output/cluster/read_cluster.F
Chd|-- called by -----------
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|-- calls ---------------
Chd|        READ_DB                       source/output/tools/read_db.F 
Chd|        READ_I_C                      ../common_source/tools/input_output/write_routtines.c
Chd|        CLUSTER_MOD                   share/modules/cluster_mod.F   
Chd|====================================================================
      SUBROUTINE READ_CLUSTER(CLUSTER)
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
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTER
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,IL,RL,ITY,NEL,NNOD,IFAIL
      INTEGER ILCLUSTER_MAX, RLCLUSTER_MAX
      my_real, DIMENSION(:), ALLOCATABLE :: RCLTAB
      INTEGER, DIMENSION(:), ALLOCATABLE :: ICLTAB
      INTEGER, DIMENSION(NCLUSTER) :: ILCLUSTER,RLCLUSTER

C=======================================================================

      CALL READ_I_C(ILCLUSTER,NCLUSTER)
      CALL READ_I_C(RLCLUSTER,NCLUSTER)
      RLCLUSTER_MAX = 0
      ILCLUSTER_MAX = 0
      DO I = 1, NCLUSTER
        IF ( RLCLUSTER_MAX < RLCLUSTER(I) ) THEN 
          RLCLUSTER_MAX = RLCLUSTER(I)
        ENDIF 
        IF ( ILCLUSTER_MAX < ILCLUSTER(I) ) THEN 
          ILCLUSTER_MAX = ILCLUSTER(I)
        ENDIF 
      ENDDO

      ALLOCATE (ICLTAB(ILCLUSTER_MAX)) ! Integer CLuster TABle
      ALLOCATE (RCLTAB(RLCLUSTER_MAX)) ! Real CLuster TABle

      DO I = 1, NCLUSTER
        RCLTAB = ZERO
        ICLTAB = 0
        CALL READ_DB(RCLTAB,RLCLUSTER(I))
        CALL READ_I_C(ICLTAB,ILCLUSTER(I))
        IL = 0
        RL = 0
        CLUSTER(I)%ID      =   ICLTAB(IL+1) 
        CLUSTER(I)%TYPE    =   ICLTAB(IL+2) 
        CLUSTER(I)%IFAIL   =   ICLTAB(IL+3) 
        CLUSTER(I)%IGR     =   ICLTAB(IL+4) 
        CLUSTER(I)%NEL     =   ICLTAB(IL+5) 
        CLUSTER(I)%NNOD    =   ICLTAB(IL+6) 
        CLUSTER(I)%SKEW    =   ICLTAB(IL+7) 
        CLUSTER(I)%OFF     =   ICLTAB(IL+8) 
        IL = IL + 8
        CLUSTER(I)%FAIL    =   RCLTAB(RL+1) 
        RL = RL + 1
        NEL   = CLUSTER(I)%NEL
        NNOD  = CLUSTER(I)%NNOD
        IFAIL = CLUSTER(I)%IFAIL 
        ALLOCATE (CLUSTER(I)%NG(NEL))
        ALLOCATE (CLUSTER(I)%ELEM(NEL))
        ALLOCATE (CLUSTER(I)%NOD1(NNOD))
        ALLOCATE (CLUSTER(I)%NOD2(NNOD))
        DO J = 1, NEL
          CLUSTER(I)%NG(J)   =  ICLTAB(IL + J)      !element  group number
          CLUSTER(I)%ELEM(J) =  ICLTAB(IL + J+NEL)  !element index in the group
        ENDDO  ! J = 1,NEL
        IL = IL + NEL*2      

        DO J = 1, NNOD
           CLUSTER(I)%NOD1(J) = ICLTAB(IL + J)      
           CLUSTER(I)%NOD2(J) = ICLTAB(IL + J+NNOD) 
        ENDDO

        IL = IL + NNOD*2      
        IF (CLUSTER(I)%IFAIL > 0) THEN
          ALLOCATE (CLUSTER(I)%FMAX(2))            
          ALLOCATE (CLUSTER(I)%MMAX(2))     
          CLUSTER(I)%FMAX(1) = RCLTAB(RL + 1)  
          CLUSTER(I)%FMAX(2) = RCLTAB(RL + 2)  
          CLUSTER(I)%MMAX(1) = RCLTAB(RL + 3)  
          CLUSTER(I)%MMAX(2) = RCLTAB(RL + 4)  
          RL = RL + 4
        ELSE 
          ALLOCATE (CLUSTER(I)%FMAX(2)) 
          ALLOCATE (CLUSTER(I)%MMAX(2)) 
        ENDIF
        IF (CLUSTER(I)%IFAIL == 3) THEN
          ALLOCATE (CLUSTER(I)%AX(4)) 
          ALLOCATE (CLUSTER(I)%NX(4))
          CLUSTER(I)%AX(1) =  RCLTAB(RL + 1) 
          CLUSTER(I)%AX(2) =  RCLTAB(RL + 2) 
          CLUSTER(I)%AX(3) =  RCLTAB(RL + 3) 
          CLUSTER(I)%AX(4) =  RCLTAB(RL + 4) 
          CLUSTER(I)%NX(1) =  RCLTAB(RL + 5) 
          CLUSTER(I)%NX(2) =  RCLTAB(RL + 6) 
          CLUSTER(I)%NX(3) =  RCLTAB(RL + 7) 
          CLUSTER(I)%NX(4) =  RCLTAB(RL + 8) 
        ELSE                                        
          ALLOCATE (CLUSTER(I)%AX(0))                                    
          ALLOCATE (CLUSTER(I)%NX(0))             
        ENDIF
      ENDDO  !  I = 1, NCLUSTER_L

      DEALLOCATE (RCLTAB)
      DEALLOCATE (ICLTAB)

C-----------
      RETURN
      END
