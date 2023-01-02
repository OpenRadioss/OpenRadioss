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
Chd|  ST_QAPRINT_CLUSTERS           source/output/qaprint/st_qaprint_clusters.F
Chd|-- called by -----------
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|-- calls ---------------
Chd|        FRETITL2                      source/starter/freform.F      
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|====================================================================
      SUBROUTINE ST_QAPRINT_CLUSTERS(NOM_OPT   ,INOM_OPT  ,CLUSTERS )
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE QA_OUT_MOD
      USE CLUSTER_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "tabsiz_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NOM_OPT(LNOPT1,SNOM_OPT1), INOM_OPT(SINOM_OPT)
      TYPE (CLUSTER_) ,DIMENSION(NCLUSTER) :: CLUSTERS
C-----------------------------------------------
C      NOM_OPT(LNOPT1,SNOM_OPT1) 
C         * Possibly, NOM_OPT(1) = ID
C        NOM_OPT(LNOPT1-LTITL+1:LTITL) <=> TITLES of the OPTIONS
C--------------------------------------------------
C      SNOM_OPT1= NRBODY+NACCELM+NVOLU+NINTER+NINTSUB+
C     +           NRWALL+NJOINT+NSECT+NLINK+
C     +           NUMSKW+1+NUMFRAM+1+NFXBODY+NFLOW+NRBE2+
C     +           NRBE3+NSUBMOD+NFXVEL+NUMBCS+NUMMPC+
C     +           NGJOINT+NUNIT0+NFUNCT+NADMESH+
C     +           NSPHIO+NSPCOND+NRBYKIN+NEBCS+
C     +           NINICRACK+NODMAS+NBGAUGE+NCLUSTER+NINTERFRIC+
C     +           NRBMERGE
C-----------------------------------------------
C      INOM_OPT(SINOM_OPT)
C--------------------------------------------------
C      INOM_OPT(1) = NRBODY
C      INOM_OPT(2) = INOM_OPT(1) + NACCELM
C      INOM_OPT(3) = INOM_OPT(2) + NVOLU
C      INOM_OPT(4) = INOM_OPT(3) + NINTER
C      INOM_OPT(5) = INOM_OPT(4) + NINTSUB
C      INOM_OPT(6) = INOM_OPT(5) + NRWALL
C      INOM_OPT(7) = INOM_OPT(6) 
C      INOM_OPT(8) = INOM_OPT(7) + NJOINT
C      INOM_OPT(9) = INOM_OPT(8) + NSECT
C      INOM_OPT(10)= INOM_OPT(9) + NLINK
C      INOM_OPT(11)= INOM_OPT(10)+ NUMSKW+1+NUMFRAM+1+NSUBMOD
C      INOM_OPT(12)= INOM_OPT(11)+ NFXBODY
C      INOM_OPT(13)= INOM_OPT(12)+ NFLOW
C      INOM_OPT(14)= INOM_OPT(13)+ NRBE2
C      INOM_OPT(15)= INOM_OPT(14)+ NRBE3
C      INOM_OPT(16)= INOM_OPT(15)+ NFXVEL
C      INOM_OPT(17)= INOM_OPT(16)+ NUMBCS
C      INOM_OPT(18)= INOM_OPT(17)+ NUMMPC
C      INOM_OPT(19)= INOM_OPT(18)+ NGJOINT
C      INOM_OPT(20)= INOM_OPT(19)+ NUNIT0
C      INOM_OPT(21)= INOM_OPT(20)+ NFUNCT
C      INOM_OPT(22)= INOM_OPT(21)+ NADMESH
C      INOM_OPT(23)= INOM_OPT(22)+ NSPHIO
C      INOM_OPT(24)= INOM_OPT(23)+ NSPCOND
C      INOM_OPT(25)= INOM_OPT(24)+ NEBCS
C      INOM_OPT(26)= INOM_OPT(25)+ NINICRACK
C      INOM_OPT(27)= INOM_OPT(26)+ NODMAS
C      INOM_OPT(28)= INOM_OPT(27)+ NBGAUGE
C      INOM_OPT(29)= INOM_OPT(28)+ NCLUSTER
C      INOM_OPT(30)= INOM_OPT(29)+ NINTERFRIC
C      INOM_OPT(31)= INOM_OPT(30)+ NRBMERGE
C      .. TO BE MAINTAINED (cf doc/inom_opt.txt) ..
C-----------------------------------------------
C--------------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MY_ID, MY_CLUSTER, TEMP_INT
      CHARACTER *nchartitle TITR
      CHARACTER (LEN=255) :: VARNAME
      DOUBLE PRECISION TEMP_DOUBLE
C-----------------------------------------------
C     /CLUSTER
C-----------------------------------------------
      IF (MYQAKEY('/CLUSTER')) THEN
        DO MY_CLUSTER=1,NCLUSTER
C        
          TITR(1:nchartitle)=''
          MY_ID = CLUSTERS(MY_CLUSTER)%ID
          CALL FRETITL2(TITR,NOM_OPT(LNOPT1-LTITR+1,INOM_OPT(28) + MY_CLUSTER),LTITR)
          IF(LEN_TRIM(TITR)/=0)THEN
            CALL QAPRINT(TITR(1:LEN_TRIM(TITR)),MY_ID,0.0_8)
          ELSE
            CALL QAPRINT('A_CLUSTER_FAKE_NAME', MY_ID,0.0_8)
          END IF
C
          WRITE(VARNAME,'(A)') 'CLUSTER_ELGROUP'
          TEMP_INT = CLUSTERS(MY_CLUSTER)%IGR
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          WRITE(VARNAME,'(A)') 'CLUSTER_TYPE'
          TEMP_INT = CLUSTERS(MY_CLUSTER)%TYPE
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          WRITE(VARNAME,'(A)') 'CLUSTER_SKEW'
          TEMP_INT = CLUSTERS(MY_CLUSTER)%SKEW
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          WRITE(VARNAME,'(A)') 'CLUSTER_NEL'
          TEMP_INT = CLUSTERS(MY_CLUSTER)%NEL
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          DO I = 1, CLUSTERS(MY_CLUSTER)%NEL
             WRITE(VARNAME,'(A,I0)') 'CLUSTER_ELEM_',I
             TEMP_INT = CLUSTERS(MY_CLUSTER)%ELEM(I)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                         
          ENDDO
C
          WRITE(VARNAME,'(A)') 'CLUSTER_IFAIL'
          TEMP_INT = CLUSTERS(MY_CLUSTER)%IFAIL
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          WRITE(VARNAME,'(A)') 'CLUSTER_OFF'
          TEMP_INT = CLUSTERS(MY_CLUSTER)%OFF
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          WRITE(VARNAME,'(A)') 'CLUSTER_FAIL'
          TEMP_DOUBLE = CLUSTERS(MY_CLUSTER)%FAIL
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
C
          WRITE(VARNAME,'(A)') 'CLUSTER_NNOD'
          TEMP_INT = CLUSTERS(MY_CLUSTER)%NNOD
          CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)
C
          DO I = 1, CLUSTERS(MY_CLUSTER)%NNOD
             WRITE(VARNAME,'(A,I0)') 'CLUSTER_NNOD1_',I
             TEMP_INT = CLUSTERS(MY_CLUSTER)%NOD1(I)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                         
          ENDDO
C
          DO I = 1, CLUSTERS(MY_CLUSTER)%NNOD
             WRITE(VARNAME,'(A,I0)') 'CLUSTER_NNOD2_',I
             TEMP_INT = CLUSTERS(MY_CLUSTER)%NOD2(I)
             CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),TEMP_INT,0.0_8)                         
          ENDDO
C
          DO I = 1,2
            WRITE(VARNAME,'(A,I0)') 'CLUSTER_FMAX',I
            TEMP_DOUBLE = CLUSTERS(MY_CLUSTER)%FMAX(I)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
C
          DO I = 1,2
            WRITE(VARNAME,'(A,I0)') 'CLUSTER_MMAX',I
            TEMP_DOUBLE = CLUSTERS(MY_CLUSTER)%MMAX(I)
            CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
          ENDDO
C
          IF (CLUSTERS(MY_CLUSTER)%IFAIL == 3) THEN
C
            DO I = 1,4
              WRITE(VARNAME,'(A,I0)') 'CLUSTER_AX',I
              TEMP_DOUBLE = CLUSTERS(MY_CLUSTER)%AX(I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            ENDDO
C            
            DO I = 1,4
              WRITE(VARNAME,'(A,I0)') 'CLUSTER_NX',I
              TEMP_DOUBLE = CLUSTERS(MY_CLUSTER)%NX(I)
              CALL QAPRINT(VARNAME(1:LEN_TRIM(VARNAME)),0,TEMP_DOUBLE)
            ENDDO
C          
          ENDIF
C
        END DO
      END IF
C-----------------------------------------------
      RETURN
      END
